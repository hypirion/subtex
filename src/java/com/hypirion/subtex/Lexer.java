package com.hypirion.subtex;

import clojure.lang.Keyword;
import java.util.function.Function;
import java.util.function.BiFunction;

// DFA of a subset of LaTeX.
class Lexer {

    enum State {
        Continue,
        End,
        None
    }

    enum Token {
        Text("text"),
        ParaEnd("para-end"),
        Call("call"),
        OpenBrace("open-brace"),
        CloseBrace("close-brace"),
        Quoted("quoted"),
        Comment("comment");

        Token(String kw) {
            this.keyword = Keyword.intern(null, kw);
        }
        Keyword keyword;
        // TODO? Math
    }

    private enum StateFn {
        Begin((a, b) -> a.stateBegin(b)),
        Slash0((a, b) -> a.stateSlash0(b)),
        Slash((a, b) -> a.stateSlash(b)),
        InComment((a, b) -> a.stateInComment(b)),
        Text((a, b) -> a.stateText(b)),
        TextNewline((a, b) -> a.stateTextNewline(b)),
        Newline0((a, b) -> a.stateNewline0(b)),
        Newlines((a, b) -> a.stateNewlines(b));

        StateFn(BiFunction<Lexer, Character, State> bi) {
            this.fn = bi;
        }

        BiFunction<Lexer, Character, State> fn;
    }

    Token token;
    StateFn state;
    int backtrack;

    Lexer() {
        state = StateFn.Begin;
        backtrack = 0;
    }

    State state(Character c) {
        return state.fn.apply(this, c);
    }

    void reset() {
        state = StateFn.Begin;
        backtrack = 0;
    }

    State eof() {
        return State.None;
    }

    State stateBegin(Character c) {
        switch (c) {
        case '\n':
            state = StateFn.Newline0;
            return State.Continue;
        case '{':
            token = Token.OpenBrace;
            return State.End;
        case '}':
            token = Token.CloseBrace;
            return State.End;
        case '\\':
            state = StateFn.Slash0;
            return State.Continue;
        case '%':
            state = StateFn.InComment;
            return State.Continue;
        default:
            state = StateFn.Text;
            return State.Continue;
        }
    }

    State stateSlash0(Character c) {
        if (Character.isLetter(c)) {
            state = StateFn.Slash;
            return State.Continue;
        }
        // Token may or may not be unexpected; this is up to the user.
        token = Token.Quoted;
        return State.End;
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
            state = StateFn.Begin;
            token = Token.Comment;
            backtrack = 1;
            return State.End;
        }
        return State.Continue;
    }

    State stateText(Character c) {
        switch (c) {
        case '{': case '}': case '%': case '\\':
            token = Token.Text;
            backtrack = 1;
            return State.End;
        case '\n':
            state = StateFn.TextNewline;
            return State.Continue;
        }
        return State.Continue;
    }

    State stateTextNewline(Character c) {
        switch (c) {
        case '{': case '}': case '%': case '\\':
            token = Token.Text;
            backtrack = 1;
            return State.End;
        case '\n':
            token = Token.Text;
            backtrack = 2;
            return State.End;
        }
        state = StateFn.Text;
        return State.Continue;
    }

    State stateNewline0(Character c) {
        switch (c) {
        case '{': case '}': case '%': case '\\':
            token = Token.Text;
            backtrack = 1;
            return State.End;
        case '\n':
            state = StateFn.Newlines;
            return State.Continue;

        }
        state = StateFn.Begin;
        return State.Continue;
    }

    State stateNewlines(Character c) {
        if (c == '\n') {
            return State.Continue;
        } else {
            backtrack = 1;
            token = Token.ParaEnd;
            return State.End;
        }
    }
}
