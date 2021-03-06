/* -------------------------------------------------------------------
 * Copyright (c) 2006 Wael Chatila / Icegreen Technologies. All Rights Reserved.
 * This software is released under the LGPL which is available at http://www.gnu.org/copyleft/lesser.html
 * This file has been modified by the copyright holder. Original file can be found at http://james.apache.org
 * -------------------------------------------------------------------
 */
package com.icegreen.greenmail.imap;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.mail.Flags;
import javax.mail.internet.MimeMessage;

import com.icegreen.greenmail.foedus.util.MsgRangeFilter;
import com.icegreen.greenmail.imap.commands.search.Criteria;
import com.icegreen.greenmail.mail.MovingMessage;
import com.icegreen.greenmail.store.FolderException;
import com.icegreen.greenmail.store.FolderListener;
import com.icegreen.greenmail.store.MailFolder;
import com.icegreen.greenmail.store.SimpleStoredMessage;
import com.icegreen.greenmail.store.InMemoryStore;

import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.Symbol;
import clojure.lang.IFn;
import clojure.lang.AFn;
import clojure.lang.IDeref;
import clojure.lang.Keyword;

public class ImapSessionFolder implements MailFolder, FolderListener {
    private MailFolder _folder;
    private ImapSession _session;
    private boolean _readonly;
    private boolean _sizeChanged;
    private List<Integer> _expungedMsns = Collections.synchronizedList(new LinkedList<Integer>());
    private Map<Integer, FlagUpdate> _modifiedFlags = Collections.synchronizedMap(new TreeMap<Integer, FlagUpdate>());

    public static Var REQUIRE = RT.var("clojure.core","require");
    public static Symbol GREENMAIL_STORE = Symbol.intern("greenmail.store");
    public static Var GET_MSN = RT.var("greenmail.store","get-msn");
    public static Keyword ID = Keyword.intern("id");

    static {
        REQUIRE.invoke(GREENMAIL_STORE);
    }

    public ImapSessionFolder(MailFolder folder, ImapSession session, boolean readonly) {
        _folder = folder;
        _session = session;
        _readonly = readonly;
        // TODO make this a weak reference (or make sure deselect() is *always* called).
        _folder.addListener(this);
    }

    public void deselect() {
        _folder.removeListener(this);
        _folder = null;
    }

    public int getMsn(long uid) throws FolderException {
        Object r = ((IFn)GET_MSN.deref()).invoke(RT.get(_folder,ID),uid);
        if (r == null)
            throw new FolderException("No such message.");
        return ((Long)r).intValue();
    }

    public void signalDeletion() {
        _folder.signalDeletion();
    }

    public List<SimpleStoredMessage> getMessages(MsgRangeFilter msgRangeFilter) {
        return _folder.getMessages(msgRangeFilter);
    }

    public List<SimpleStoredMessage> getMessages() {
        return _folder.getMessages();
    }

    public List<SimpleStoredMessage> getNonDeletedMessages() {
        return _folder.getNonDeletedMessages();
    }

    public boolean isReadonly() {
        return _readonly;
    }

    public int[] getExpunged() {
        synchronized (_expungedMsns) {
            int[] expungedMsns = new int[_expungedMsns.size()];
            for (int i = 0; i < expungedMsns.length; i++) {
                int msn = _expungedMsns.get(i);
                expungedMsns[i] = msn;
            }
            _expungedMsns.clear();

            // TODO - renumber any cached ids (for now we assume the _modifiedFlags has been cleared)\
            if (!(_modifiedFlags.isEmpty() && !_sizeChanged)) {
                throw new IllegalStateException("Need to do this properly...");
            }
            return expungedMsns;
        }
    }

    public List<FlagUpdate> getFlagUpdates() {
        if (_modifiedFlags.isEmpty()) {
            return Collections.EMPTY_LIST;
        }

        List<FlagUpdate> retVal = new ArrayList<FlagUpdate>();
        retVal.addAll(_modifiedFlags.values());
        _modifiedFlags.clear();
        return retVal;
    }

    public void expunged(int msn) {
        synchronized (_expungedMsns) {
            _expungedMsns.add(new Integer(msn));
        }
    }

    public void added(int msn) {
        _sizeChanged = true;
    }

    public void flagsUpdated(int msn, Flags flags, Long uid) {
        // This will overwrite any earlier changes
        _modifiedFlags.put(new Integer(msn), new FlagUpdate(msn, uid, flags));
    }

    public void mailboxDeleted() {
        _session.closeConnection("Mailbox " + _folder.getName() + " has been deleted");
    }

    public String getName() {
        return _folder.getName();
    }

    public String getFullName() {
        return _folder.getFullName();
    }

    public Flags getPermanentFlags() {
        return _folder.getPermanentFlags();
    }

    public int getMessageCount() {
        return _folder.getMessageCount();
    }

    public int getRecentCount(boolean reset) {
        return _folder.getRecentCount(reset);
    }

    public long getUidValidity() {
        return _folder.getUidValidity();
    }

    public int getFirstUnseen() {
        return correctForExpungedMessages(_folder.getFirstUnseen());
    }

    /**
     * Adjust an actual mailbox msn for the expunged messages in this mailbox that have not
     * yet been notified.
     * TODO - need a test for this
     */
    private int correctForExpungedMessages(int absoluteMsn) {
        int correctedMsn = absoluteMsn;
        // Loop throught the expunged list backwards, adjusting the msn as we go.
        for (int i = (_expungedMsns.size() - 1); i >= 0; i--) {
            Integer expunged = _expungedMsns.get(i);
            if (expunged.intValue() <= absoluteMsn) {
                correctedMsn++;
            }
        }
        return correctedMsn;
    }

    public boolean isSelectable() {
        return _folder.isSelectable();
    }

    public long getUidNext() {
        return _folder.getUidNext();
    }

    public int getUnseenCount() {
        return _folder.getUnseenCount();
    }

    public long appendMessage(MimeMessage message, Flags flags, Date internalDate) {
        return _folder.appendMessage(message, flags, internalDate);
    }

    public void store(MovingMessage mail) throws Exception {
        _folder.store(mail);
    }

    public void store(MimeMessage mail) throws Exception {
        _folder.store(mail);
    }

    public void store(MimeMessage mail, Date internalDate) throws Exception {
        _folder.store(mail, internalDate);
    }
    
    public SimpleStoredMessage getMessage(long uid) {
        return _folder.getMessage(uid);
    }

    public long[] getMessageUids() {
        return _folder.getMessageUids();
    }

    public void expunge() throws FolderException {
        InMemoryStore.expunge(_folder);
    }

    public long[] search(Criteria searchTerm) {
        return _folder.search(searchTerm);
    }

    public long copyMessage(long uid, MailFolder toFolder) throws FolderException {
        return _folder.copyMessage(uid, toFolder);
    }

    public void addListener(FolderListener listener) {
        _folder.addListener(listener);
    }

    public void removeListener(FolderListener listener) {
        _folder.removeListener(listener);
    }

    public void setFlags(Flags flags, boolean value, long uid, FolderListener silentListener, boolean addUid) throws FolderException {
        _folder.setFlags(flags, value, uid, silentListener, addUid);
    }

    public void replaceFlags(Flags flags, long uid, FolderListener silentListener, boolean addUid) throws FolderException {
        _folder.replaceFlags(flags, uid, silentListener, addUid);
    }

    public void deleteAllMessages() {
        _folder.deleteAllMessages();
    }

    public boolean isSizeChanged() {
        return _sizeChanged;
    }

    public void setSizeChanged(boolean sizeChanged) {
        _sizeChanged = sizeChanged;
    }

    static final class FlagUpdate {
        private int msn;
        private Long uid;
        private Flags flags;

        public FlagUpdate(int msn, Long uid, Flags flags) {
            this.msn = msn;
            this.uid = uid;
            this.flags = flags;
        }

        public int getMsn() {
            return msn;
        }

        public Long getUid() {
            return uid;
        }

        public Flags getFlags() {
            return flags;
        }
    }

}
