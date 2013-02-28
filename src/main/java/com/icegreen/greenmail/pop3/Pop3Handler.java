/*
 * Copyright (c) 2006 Wael Chatila / Icegreen Technologies. All Rights Reserved.
 * This software is released under the LGPL which is available at http://www.gnu.org/copyleft/lesser.html
 * This file has been used and modified. Original file can be found on http://foedus.sourceforge.net
 */
package com.icegreen.greenmail.pop3;


import com.icegreen.greenmail.pop3.commands.Pop3Command;
import com.icegreen.greenmail.user.UserManager;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.StringTokenizer;

import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.Symbol;
import clojure.lang.IFn;
import clojure.lang.IDeref;

public class Pop3Handler extends Thread {

    public static Var REQUIRE = RT.var("clojure.core","require");
    public static Var ASSOC = RT.var("clojure.core","assoc");
    public static Var SWAP = RT.var("clojure.core","swap!");
    public static Symbol GREENMAIL_POP3 = Symbol.intern("greenmail.pop3");
    public static Var COMMANDS = RT.var("greenmail.pop3","commands");

    static {
        try {
            REQUIRE.invoke(GREENMAIL_POP3);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static void registerCommand(String name, Pop3Command command) {
        ((IFn)SWAP.deref()).invoke(COMMANDS.deref(), ASSOC.deref(), name, command);
    }

    public static Pop3Command getCommand(String name) {
        return (Pop3Command)RT.get(((IDeref)COMMANDS.deref()).deref(),name);
    }

    Pop3Connection _conn;
    UserManager _manager;
    Pop3State _state;
    boolean _quitting;
    String _currentLine;
    private Socket _socket;

    public Pop3Handler(UserManager manager, Socket socket) {
        _manager = manager;
        _socket = socket;
    }

    public void run() {
        Pop3Server.pop3_lock.lock();
        try {
            _conn = new Pop3Connection(this, _socket);
            _state = new Pop3State(_manager);

            _quitting = false;

            sendGreetings();

            while (!_quitting) {
                handleCommand();
            }

            _conn.close();
        } catch (SocketTimeoutException ste) {
            _conn.println("421 Service shutting down and closing transmission channel");

        } catch (Exception e) {
            //e.printStackTrace();
        } finally {
            try {
                _socket.close();
            } catch (IOException ioe) {
                //ioe.printStackTrace();
            }
            Pop3Server.pop3_lock.unlock();
        }
    }

    void sendGreetings() {
        _conn.println("+OK POP3 GreenMail Server ready");
    }

    void handleCommand()
        throws IOException {
        _currentLine = _conn.readLine();

        if (_currentLine == null) {
            quit();

            return;
        }

        String commandName = new StringTokenizer(_currentLine, " ").nextToken()
            .toUpperCase();

        Pop3Command command = getCommand(commandName);

        if (command == null) {
            _conn.println("-ERR Command not recognized");

            return;
        }

        if (!command.isValidForState(_state)) {
            _conn.println("-ERR Command not valid for this state");

            return;
        }

        command.execute(_conn, _state, _currentLine);
    }

    public void quit() {
        _quitting = true;
        try {
            if (_socket != null && !_socket.isClosed()) {
                _socket.close();
            }
        } catch(IOException ignored) {
            //empty
        }
    }
}
