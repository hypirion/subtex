package com.hypirion.subtex;

import static com.hypirion.subtex.Lexer.State;
import static com.hypirion.subtex.Lexer.Token;

import com.hypirion.subtex.records.Item;
import clojure.lang.Keyword;
import java.util.Iterator;

public abstract class Tokenise implements Iterator<Item> {
    public Tokenise(String value) {

    }
}
