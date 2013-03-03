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
import clojure.lang.Keyword;

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
    public static Var GET_CHILDREN = RT.var("greenmail.store","get-children");
    public static Var GET_CHILD = RT.var("greenmail.store","get-child");
    public static Var ADD_CHILD = RT.var("greenmail.store","add-child");
    public static Var REMOVE_CHILD = RT.var("greenmail.store","remove-child");
    public static Var GET_PARENT = RT.var("greenmail.store","get-parent");
    public static Var SET_NAME = RT.var("greenmail.store","set-name");
    public static Var SET_SELECTABLE = RT.var("greenmail.store","set-selectable");
    public static Var MAIL_FOLDER = RT.var("greenmail.store","mail-folder");
    public static Var EXPUNGE = RT.var("greenmail.store","expunge");
    public static Var GET_MAILBOX = RT.var("greenmail.store","get-mailbox");
    public static Var HIMF = RT.var("greenmail.store","->HiMF");
    public static Var CREATE_MAILBOX = RT.var("greenmail.store","create-mailbox");
    public static Keyword ID = Keyword.intern("id");
    
    public MailFolder rootMailbox = createRootFolder();
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
        Object r = ((IFn)GET_MAILBOX.deref()).invoke(this,absoluteMailboxName);
        if (r == null)
            return null;
        return (MailFolder)((IFn)HIMF.deref()).invoke(r);
    }

    public MailFolder getMailbox(MailFolder parent, String name) {
        Object r = (((IFn)GET_MAILBOX.deref()).invoke(this,parent,name));
        if (r == null)
            return null;
        return (MailFolder)((IFn)HIMF.deref()).invoke(r);
    }

    public MailFolder createMailbox(MailFolder parent,
                                    String mailboxName,
                                    boolean selectable)
        throws FolderException {
        return (MailFolder)((IFn)CREATE_MAILBOX.deref()).invoke(parent, mailboxName, selectable);
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
        return (MailFolder)MAIL_FOLDER.invoke(null,ImapConstants.USER_NAMESPACE, true);
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
        return (MailFolder)MAIL_FOLDER.invoke(parent, name);
    }

    public static void expunge (MailFolder mf){
        ((IFn)EXPUNGE.deref()).invoke(RT.get(mf, ID));
    }
}
