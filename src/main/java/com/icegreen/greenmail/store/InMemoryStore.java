/* -------------------------------------------------------------------
 * Copyright (c) 2006 Wael Chatila / Icegreen Technologies. All Rights Reserved.
 * This software is released under the LGPL which is available at http://www.gnu.org/copyleft/lesser.html
 * This file has been modified by the copyright holder. Original file can be found at http://james.apache.org
 * -------------------------------------------------------------------
 */
package com.icegreen.greenmail.store;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import javax.mail.Flags;
import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import com.icegreen.greenmail.foedus.util.MsgRangeFilter;
import com.icegreen.greenmail.imap.ImapConstants;
import com.icegreen.greenmail.imap.commands.search.Criteria;
import com.icegreen.greenmail.mail.MovingMessage;

import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.Symbol;
import clojure.lang.IFn;
import clojure.lang.AFn;
import clojure.lang.IDeref;

/**
 * A simple in-memory implementation of {@link Store}, used for testing
 * and development. Note: this implementation does not persist *anything* to disk.
 *
 * @author Darrell DeBoer <darrell@apache.org>
 * @version $Revision: 109034 $
 */
public class InMemoryStore
    implements Store, ImapConstants {

    public static Var REQUIRE = RT.var("clojure.core","require");
    public static Symbol GREENMAIL_STORE = Symbol.intern("greenmail.store");
    public static Var EXPUNGE = RT.var("greenmail.store","expunge");
    public static Var GET_CHILDREN = RT.var("greenmail.store","get-children");
    public static Var GET_CHILD = RT.var("greenmail.store","get-child");
    public static Var ADD_CHILD = RT.var("greenmail.store","add-child");
    public static Var REMOVE_CHILD = RT.var("greenmail.store","remove-child");
    public static Var GET_PARENT = RT.var("greenmail.store","get-parent");
    public static Var SET_NAME = RT.var("greenmail.store","set-name");
    public static Var SET_SELECTABLE = RT.var("greenmail.store","set-selectable");
    public static Var MAIL_FOLDER = RT.var("greenmail.store","mail-folder");
    
    private MailFolder rootMailbox = createRootFolder();
    private static final Flags PERMANENT_FLAGS = new Flags();

    static {
        PERMANENT_FLAGS.add(Flags.Flag.ANSWERED);
        PERMANENT_FLAGS.add(Flags.Flag.DELETED);
        PERMANENT_FLAGS.add(Flags.Flag.DRAFT);
        PERMANENT_FLAGS.add(Flags.Flag.FLAGGED);
        PERMANENT_FLAGS.add(Flags.Flag.SEEN);
        REQUIRE.invoke(GREENMAIL_STORE);
    }

    public MailFolder getMailbox(String absoluteMailboxName) {
        StringTokenizer tokens = new StringTokenizer(absoluteMailboxName, HIERARCHY_DELIMITER);

        // The first token must be "#mail"
        if (!tokens.hasMoreTokens() ||
            !tokens.nextToken().equalsIgnoreCase(USER_NAMESPACE)) {
            return null;
        }

        MailFolder parent = rootMailbox;
        while (parent != null && tokens.hasMoreTokens()) {
            String childName = tokens.nextToken();
            parent = getChild(parent, childName);
        }
        return parent;
    }

    public MailFolder getMailbox(MailFolder parent, String name) {
        return getChild(parent, name);
    }

    public MailFolder createMailbox(MailFolder parent,
                                    String mailboxName,
                                    boolean selectable)
        throws FolderException {
        if (mailboxName.indexOf(HIERARCHY_DELIMITER_CHAR) != -1) {
            throw new FolderException("Invalid mailbox name.");
        }
        MailFolder child = createHFolder(parent, mailboxName);
        addChild(parent, child);
        setSelectable(child,selectable);
        return child;
    }

    public void deleteMailbox(MailFolder folder) throws FolderException {
        MailFolder toDelete = folder;

        if (!getChildren_(toDelete).isEmpty()) {
            throw new FolderException("Cannot delete mailbox with children.");
        }

        if (toDelete.getMessageCount() != 0) {
            throw new FolderException("Cannot delete non-empty mailbox");
        }

        MailFolder parent = getParent(toDelete);
        removeChild(parent, toDelete);
    }

    public void renameMailbox(MailFolder existingFolder, String newName) throws FolderException {
        MailFolder toRename = existingFolder;
        setName(toRename, newName);
    }

    public Collection<MailFolder> getChildren(MailFolder parent) {
        Collection<MailFolder> children = getChildren_(parent);
        return Collections.unmodifiableCollection(children);
    }

    public MailFolder setSelectable(MailFolder folder, boolean selectable) {
        ((IFn)SET_SELECTABLE.deref()).invoke(folder, selectable);
        return folder;
    }

    /**
     * @see com.icegreen.greenmail.store.Store#listMailboxes
     */
    public Collection<MailFolder> listMailboxes(String searchPattern)
        throws FolderException {
        int starIndex = searchPattern.indexOf('*');
        int percentIndex = searchPattern.indexOf('%');

        // We only handle wildcard at the end of the search pattern.
        if ((starIndex > -1 && starIndex < searchPattern.length() - 1) ||
            (percentIndex > -1 && percentIndex < searchPattern.length() - 1)) {
            throw new FolderException("WIldcard characters are only handled as the last character of a list argument.");
        }

        ArrayList<MailFolder> mailboxes = new ArrayList<MailFolder>();
        if (starIndex != -1 || percentIndex != -1) {
            int lastDot = searchPattern.lastIndexOf(HIERARCHY_DELIMITER);
            String parentName;
            if (lastDot < 0) {
                parentName = USER_NAMESPACE;
            } else {
                parentName = searchPattern.substring(0, lastDot);
            }
            String matchPattern = searchPattern.substring(lastDot + 1, searchPattern.length() - 1);

            MailFolder parent = (MailFolder) getMailbox(parentName);
            // If the parent from the search pattern doesn't exist,
            // return empty.
            if (parent != null) {
                Iterator<MailFolder> children = new ArrayList<MailFolder>(getChildren_(parent)).iterator();
                while (children.hasNext()) {
                    MailFolder child = children.next();
                    if (child.getName().startsWith(matchPattern)) {
                        mailboxes.add(child);

                        if (starIndex != -1) {
                            addAllChildren(child, mailboxes);
                        }
                    }
                }
            }

        } else {
            MailFolder folder = getMailbox(searchPattern);
            if (folder != null) {
                mailboxes.add(folder);
            }
        }

        return mailboxes;
    }

    private void addAllChildren(MailFolder mailbox, Collection<MailFolder> mailboxes) {
        Collection<MailFolder> children = getChildren_(mailbox);
        Iterator<MailFolder> iterator = new ArrayList<MailFolder>(children).iterator();
        while (iterator.hasNext()) {
            MailFolder child = iterator.next();
            mailboxes.add(child);
            addAllChildren(child, mailboxes);
        }
    }

    public static MailFolder createRootFolder() {
        //return new RootFolder();
        return (MailFolder)MAIL_FOLDER.invoke(null,ImapConstants.USER_NAMESPACE, true);
    }

    private static class RootFolder extends HierarchicalFolder {
        public RootFolder() {
            super(null, ImapConstants.USER_NAMESPACE);
        }

        public String getFullName() {
            return name;
        }
    }

    public static Collection<MailFolder> getChildren_(Object folder) {
        return (Collection<MailFolder>)(((IFn)GET_CHILDREN.deref()).invoke(folder));
    }

    public static MailFolder getChild(Object folder, String name) {
        return (MailFolder)(((IFn)GET_CHILD.deref()).invoke(folder,name));
    }

    public static void addChild(Object folder, Object child) {
        ((IFn)ADD_CHILD.deref()).invoke(folder,child);
    }

    public static void removeChild(Object folder, Object child) {
        ((IFn)REMOVE_CHILD.deref()).invoke(folder,child);
    }

    public static MailFolder getParent(Object folder) {
        return (MailFolder)(((IFn)GET_PARENT.deref()).invoke(folder));
    }

    public static void setName(Object folder, String name) {
        ((IFn)SET_NAME.deref()).invoke(folder,name);
    }

    public static MailFolder createHFolder(MailFolder parent, String name) {
        return new HierarchicalFolder(parent,name);
    }

    public static class HierarchicalFolder implements MailFolder {
        private Collection<MailFolder> children;
        private MailFolder parent;

        protected String name;
        private boolean isSelectable = false;

        private List<SimpleStoredMessage> mailMessages = Collections.synchronizedList(new LinkedList<SimpleStoredMessage>());
        private long nextUid = 1;
        private long uidValidity;

        private List<FolderListener> _mailboxListeners = Collections.synchronizedList(new LinkedList<FolderListener>());

        public HierarchicalFolder(MailFolder parent,
                                  String name) {
            this.name = name;
            this.children = new ArrayList<MailFolder>();
            this.parent = parent;
            this.uidValidity = System.currentTimeMillis();
        }

        public Collection<MailFolder> getChildren() {
            return children;
        }

        public MailFolder getParent() {
            return parent;
        }

        public MailFolder getChild(String name) {
            Iterator<MailFolder> iterator = children.iterator();
            while (iterator.hasNext()) {
                MailFolder child = iterator.next();
                if (child.getName().equalsIgnoreCase(name)) {
                    return child;
                }
            }
            return null;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public String getFullName() {
            return parent.getFullName() + HIERARCHY_DELIMITER_CHAR + name;
        }

        public Flags getPermanentFlags() {
            return PERMANENT_FLAGS;
        }

        public int getMessageCount() {
            return mailMessages.size();
        }

        public long getUidValidity() {
            return uidValidity;
        }

        public long getUidNext() {
            return nextUid;
        }

        public int getUnseenCount() {
            int count = 0;
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (!message.getFlags().contains(Flags.Flag.SEEN)) {
                    count++;
                }
            }
            return count;
        }

        /**
         * Returns the 1-based index of the first unseen message. Unless there are outstanding
         * expunge responses in the ImapSessionMailbox, this will correspond to the MSN for
         * the first unseen.
         */
        public int getFirstUnseen() {
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (!message.getFlags().contains(Flags.Flag.SEEN)) {
                    return i + 1;
                }
            }
            return -1;
        }

        public int getRecentCount(boolean reset) {
            int count = 0;
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (message.getFlags().contains(Flags.Flag.RECENT)) {
                    count++;
                    if (reset) {
                        message.getFlags().remove(Flags.Flag.RECENT);
                    }
                }
            }
            return count;
        }

        public int getMsn(long uid) throws FolderException {
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (message.getUid() == uid) {
                    return i + 1;
                }
            }
            throw new FolderException("No such message.");
        }

        public void signalDeletion() {
            // Notify all the listeners of the new message
            synchronized (_mailboxListeners) {
                for (int j = 0; j < _mailboxListeners.size(); j++) {
                    FolderListener listener = _mailboxListeners.get(j);
                    listener.mailboxDeleted();
                }
            }

        }

        public List<SimpleStoredMessage> getMessages(MsgRangeFilter range) {
            List<SimpleStoredMessage> ret = new ArrayList<SimpleStoredMessage>();
            for (int i = 0; i < mailMessages.size(); i++) {
                if (range.includes(i+1)) {
                    ret.add(mailMessages.get(i));
                }
            }

            return ret;
        }

        @Override
        public List<SimpleStoredMessage> getMessages() {
            return mailMessages;
        }

        public List<SimpleStoredMessage> getNonDeletedMessages() {
            List<SimpleStoredMessage> ret = new ArrayList<SimpleStoredMessage>();
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (!message.getFlags().contains(Flags.Flag.DELETED)) {
                    ret.add(message);
                }
            }
            return ret;
        }

        public boolean isSelectable() {
            return isSelectable;
        }

        public void setSelectable(boolean selectable) {
            isSelectable = selectable;
        }

        public long appendMessage(MimeMessage message,
                                  Flags flags,
                                  Date internalDate) {
            long uid = nextUid;
            nextUid++;

            //            flags.setRecent(true);
            SimpleStoredMessage storedMessage = new SimpleStoredMessage(message, flags,
                                                                        internalDate, uid);
            storedMessage.getFlags().add(Flags.Flag.RECENT);

            mailMessages.add(storedMessage);
            int newMsn = mailMessages.size();

            // Notify all the listeners of the new message
            synchronized (_mailboxListeners) {
                for (int j = 0; j < _mailboxListeners.size(); j++) {
                    FolderListener listener = _mailboxListeners.get(j);
                    listener.added(newMsn);
                }
            }

            return uid;
        }

        public void setFlags(Flags flags, boolean value, long uid, FolderListener silentListener, boolean addUid) throws FolderException {
            int msn = getMsn(uid);
            SimpleStoredMessage message = mailMessages.get(msn - 1);

            if (value) {
                message.getFlags().add(flags);
            } else {
                message.getFlags().remove(flags);
            }

            Long uidNotification = null;
            if (addUid) {
                uidNotification = new Long(uid);
            }
            notifyFlagUpdate(msn, message.getFlags(), uidNotification, silentListener);
        }

        public void replaceFlags(Flags flags, long uid, FolderListener silentListener, boolean addUid) throws FolderException {
            int msn = getMsn(uid);
            SimpleStoredMessage message = mailMessages.get(msn - 1);
            message.getFlags().remove(MessageFlags.ALL_FLAGS);
            message.getFlags().add(flags);

            Long uidNotification = null;
            if (addUid) {
                uidNotification = new Long(uid);
            }
            notifyFlagUpdate(msn, message.getFlags(), uidNotification, silentListener);
        }

        private void notifyFlagUpdate(int msn, Flags flags, Long uidNotification, FolderListener silentListener) {
            synchronized (_mailboxListeners) {
                for (int i = 0; i < _mailboxListeners.size(); i++) {
                    FolderListener listener = _mailboxListeners.get(i);

                    if (listener == silentListener) {
                        continue;
                    }

                    listener.flagsUpdated(msn, flags, uidNotification);
                }
            }
        }

        public void deleteAllMessages() {
            mailMessages.clear();
        }

        public void store(MovingMessage mail) throws Exception {
            store(mail.getMessage());
        }


        public void store(MimeMessage message) throws Exception {
            Date internalDate = new Date();
            store(message, internalDate);
        }

        public void store(MimeMessage message, Date internalDate) throws Exception {
            Flags flags = new Flags();
            appendMessage(message, flags, internalDate);
        }


        public SimpleStoredMessage getMessage(long uid) {
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (message.getUid() == uid) {
                    return message;
                }
            }
            return null;
        }

        public long[] getMessageUids() {
            long[] uids = new long[mailMessages.size()];
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                uids[i] = message.getUid();
            }
            return uids;
        }

        private void deleteMessage(int msn) {
            mailMessages.remove(msn - 1); //NOTE BY WAEL: is this really correct, the number of items in the iterating list is changed see expunge()
        }

        public long[] search(Criteria searchTerm) {
            ArrayList<SimpleStoredMessage> matchedMessages = new ArrayList<SimpleStoredMessage>();

            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (searchTerm.match(message)) {
                    matchedMessages.add(message);
                }
            }

            long[] matchedUids = new long[matchedMessages.size()];
            for (int i = 0; i < matchedUids.length; i++) {
                SimpleStoredMessage storedMessage = matchedMessages.get(i);
                long uid = storedMessage.getUid();
                matchedUids[i] = uid;
            }
            return matchedUids;
        }

        public long copyMessage(long uid, MailFolder toFolder)
            throws FolderException {
            SimpleStoredMessage originalMessage = getMessage(uid);
            MimeMessage newMime = null;
            try {
                newMime = new MimeMessage(originalMessage.getMimeMessage());
            } catch (MessagingException e) {
                // TODO chain.
                throw new FolderException("Messaging exception: " + e.getMessage());
            }
            Flags newFlags = new Flags();
            newFlags.add(originalMessage.getFlags());
            Date newDate = originalMessage.getInternalDate();

            return toFolder.appendMessage(newMime, newFlags, newDate);
        }

        public void expunge() throws FolderException {
            for (int i = 0; i < mailMessages.size(); i++) {
                SimpleStoredMessage message = mailMessages.get(i);
                if (message.getFlags().contains(Flags.Flag.DELETED)) {
                    expungeMessage(i + 1);
                }
            }
            ((IFn)EXPUNGE.deref()).invoke(this, new AFn(){
                    public Object invoke (Object arg) {
                        for (int i = 0; i < mailMessages.size(); i++) {
                            SimpleStoredMessage message = mailMessages.get(i);
                            if (arg.equals(message.getUid())) {
                                expungeMessage(i + 1);
                            }
                        }
                        return null;
                    }
                });
        }

        private void expungeMessage(int msn) {
            // Notify all the listeners of the pending delete
            synchronized (_mailboxListeners) {
                deleteMessage(msn);
                for (int j = 0; j < _mailboxListeners.size(); j++) {
                    FolderListener expungeListener = _mailboxListeners.get(j);
                    expungeListener.expunged(msn);
                }
            }

        }

        public void addListener(FolderListener listener) {
            synchronized (_mailboxListeners) {
                _mailboxListeners.add(listener);
            }
        }

        public void removeListener(FolderListener listener) {
            synchronized (_mailboxListeners) {
                _mailboxListeners.remove(listener);
            }
        }
    }
}
