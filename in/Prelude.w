let true = \t f. t;
    false = \t f. f;

    zero = \s z. z;
    succ = \x s z. s (x s z);
    plus = \x. x succ;
    times = \x. \y. x (plus y) zero;
    isZero = \n. n (\x. false) true;

    one = succ zero;
    two = succ one;
    three = plus one two;
    four = plus two two;

    pair = \x y f. f x y;
    fst  = \p. p true;
    snd  = \p. p false;

    nil  = \x. true;
    cons = pair;
    head = fst;
    tail = snd;
    null = \p. p (\x y. false);

    compose = \f g x. f (g x);

    Y = \g. (\x. g (x x)) (\x. g (x x));

    map = Y (\r f l. ((null l) nil (cons (f (head l)) (r f (tail l)))));
    id = \x. x;
    l = cons two (cons one nil)
in succ zero
