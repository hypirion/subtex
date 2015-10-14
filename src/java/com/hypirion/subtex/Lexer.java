package com.hypirion.subtex;

import clojure.lang.Keyword;
import java.util.function.Function;

// DFA of a subset of LaTeX.
class Lexer {

    enum State {
        Continue,
        End,
        Ignore,
        Error
    }

    enum Token {
        Text("text"),
        ParaEnd("para-end"),
        Call("call"),
        OpenBrace("open-brace"),
        CloseBrace("close-brace"),
        Quoted("quoted"),
        Error("error");

        Token(String kw) {
            this.keyword = Keyword.intern(null, kw);
        }
        Keyword keyword;
        // TODO? Math
    }

    Token token;
    Function<Character, State> state;
    String error;
    int backtrack;
    
    Lexer() {
        token = Token.Error;
        state = this::stateBegin;
        backtrack = 0;
        error = null;
    }

    void reset() {
        token = Token.Error;
        state = this::stateBegin;
        backtrack = 0;
        error = null;
    }

    State stateBegin(Character c) {
        if (Character.isWhitespace(c) && c != '\n') {
            return State.Ignore;
        }
        switch (c) {
        case '\n':
            state = this::stateNewline0;
            return State.Ignore;
        case '{':
            token = Token.OpenBrace;
            return State.End;
        case '}':
            token = Token.CloseBrace;
            return State.End;
        case '\\':
            state = this::stateSlash0;
            return State.Continue;
        case '%':
            state = this::stateInComment;
            return State.Ignore;
        default:
            state = this::stateText;
            return State.Continue;
        }
    }

    State stateSlash0(Character c) {
        switch (c) {
        case '{':
        case '}':
        case '%':
        case '\\':
            token = Token.Quoted;
            return State.End;
        }
        if (Character.isLetter(c)) {
            state = this::stateSlash;
            return State.Continue;
        }
        error = String.format("Unexpected character '%C' after '\\'.", c);
        return State.Error;
    }

    State stateSlash(Character c) {
        if (Character.isLetterOrDigit(c)) {
            // Remain in stateSlash until we've found something non-letter/digit
            return State.Continue;
        }
        backtrack = 1;
        token = Token.Call;
        return State.End;
    }

    State stateInComment(Character c) {
        if (c == '\n') {
            state = this::stateBegin;
            return State.Ignore;
        }
        return State.Ignore;
    }

    State stateText(Character c) {
        switch (c) {
        case '{': case '}': case '%': case '\\':
            token = Token.Text;
            backtrack = 1;
            return State.End;
        case '\n':
            state = this::stateTextNewline;
            return State.Continue;
        }
        return State.Continue;
    }

    State stateTextNewline(Character c) {
        if (c == '\n') {
            token = Token.Text;
            backtrack = 2;
            return State.End;
        }
        state = this::stateText;
        return State.Continue;
    }

    State stateNewline0(Character c) {
        if (c == '\n') {
            state = this::stateNewlines;
            return State.Ignore;
        } else {
            state = this::stateBegin;
            return State.Ignore;
        }
    }

    State stateNewlines(Character c) {
        if (c == '\n') {
            return State.Ignore;
        } else {
            backtrack = 1;
            token = Token.ParaEnd;
            return State.End;
        }
    }
}
