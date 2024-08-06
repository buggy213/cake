Grammar { 
    n_nonterminals: 15, 
    // 0 = <ebnf>
    // 1 = <goal-nonterminal>
    // 2 = <nonterminal>
    // 10 = (DoubleNewline <nonterminal>)*
    // 9 = DoubleNewline <nonterminal>
    // 8 = Nonterminal | Terminal
    // 3 = <productions>
    // 4 = <production>
    // 12 = (Bar <production>)*
    // 13 = <factor>*
    // 5 = <factor>
    // 11 = Bar <production>
    // 7 = <base>
    // 14 = <count>?
    // 6 = <count>
    // 8 = <symbol>
    productions: 
    [
        Nonempty(9, [Terminal(DoubleNewline), Nonterminal(2)]), // (DoubleNewline <nonterminals>)
        Empty(10), // (DoubleNewline <nonterminal>)* has an empty production
        Nonempty(10, [Nonterminal(10), Nonterminal(9)]), // it also has a recursive production in terms of 9 (DoubleNewline <nonterminal>)
        Nonempty(0, [Nonterminal(1), Nonterminal(10)]), // <ebnf> ::= <goal-nonterminal> (DoubleNewline <nonterminal>)*
        Nonempty(1, [Nonterminal(2)]), // <goal-nonterminal> ::= <nonterminal>
        Nonempty(2, [Nonterminal(8), Terminal(Define), Nonterminal(3)]), // <nonterminal> ::= <symbol> Define <productions>
        Nonempty(11, [Terminal(Bar), Nonterminal(4)]), // (Bar, <production>)
        Empty(12), // (Bar <production>)* has an empty production
        Nonempty(12, [Nonterminal(12), Nonterminal(11)]), // it also has a recursive production in terms of 11 (Bar <production>)
        Nonempty(3, [Nonterminal(4), Nonterminal(12)]),  // <productions> ::= <production> (Bar <production>)*
        Empty(13), // <factor>* has an empty production
        Nonempty(13, [Nonterminal(13), Nonterminal(5)]), // it also has a recursive production in terms of 5 (<factor>)
        Nonempty(4, [Nonterminal(13)]), // <production> ::= <factor>*
        Empty(14), // <count>? has an empty production
        Nonempty(14, [Nonterminal(6)]), // it also has a nonempty production in terms of 6 (<count>)
        Nonempty(5, [Nonterminal(7), Nonterminal(14)]), // <factor> ::= <base> <count>?
        Nonempty(6, [Terminal(Plus)]), // <count> ::= Plus
        Nonempty(6, [Terminal(Star)]), // | Star
        Nonempty(6, [Terminal(Question)]), // | Question
        Nonempty(7, [Nonterminal(8)]),  // <base> ::= <symbol>
        Nonempty(7, [Terminal(LParen), Nonterminal(4), Terminal(RParen)]), // | LParen <production> RParen 
        Nonempty(8, [Terminal(Nonterminal)]),  // <symbol> ::= Nonterminal
        Nonempty(8, [Terminal(Terminal)]) // | Terminal
    ], 
    goal_symbol: 0 
}