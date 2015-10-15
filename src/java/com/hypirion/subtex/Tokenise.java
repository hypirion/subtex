package com.hypirion.subtex;

import static com.hypirion.subtex.Lexer.State;
import static com.hypirion.subtex.Lexer.Token;

import com.hypirion.subtex.records.Item;
import clojure.lang.Keyword;
import java.util.Iterator;

public class Tokenise implements Iterator<Item> {
    boolean finished;
    String val;
    int start, end;
    Lexer lexer;
    Item item;

    public Tokenise(String value) {
        lexer = new Lexer();
        finished = false;
        start = 0;
        end = 0;
        val = value;
        computeItem();
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    public Item next() {
        Item it = item;
        item = null;
        computeItem();
        return it;
    }

    public boolean hasNext() {
        return item != null;
    }

    void computeItem() {
        if (finished) {
            return;
        }
        lexer.reset();
        State state = State.Ignore;
        while (start < val.length() && state == State.Ignore) {
            state = lexer.state(val.charAt(start));
            start++;
        }
        end = start;
        start -= 1;
        while (end < val.length() && state == State.Continue) {
            state = lexer.state(val.charAt(end));
            end++;
        }
        loop:
        while (true) {
            // This loop is here because I want to call eof() afterwards. Could
            // this be an issue with e.g. "}"? Hm.
            switch (state) {
            case Continue:
                if (end < val.length()) {
                    throw new RuntimeException("Illegal internal state");
                } else {
                    state = lexer.eof();
                    continue loop;
                }
            case Ignore:
                if (end < val.length()) {
                    throw new RuntimeException("Illegal internal state");
                } else { // TODO: This isn't correct. We can ignore data while
                         // still
                    finished = true;
                    item = null;
                    return;
                }
            case Error:
                finished = true;
                item = new Item(Token.Error.keyword, lexer.error);
                return;
            case End:
                end -= lexer.backtrack;
                switch (lexer.token) {
                case Text:
                case Call:
                case Quoted:
                    item = new Item(lexer.token.keyword, val.substring(start, end));
                    break;
                case ParaEnd:
                case OpenBrace:
                case CloseBrace:
                    item = new Item(lexer.token.keyword, null);
                    break;
                }
                start = end;
                return;
            }
        }
    }
}
