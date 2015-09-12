let false = \t.\f.f;
    true  = \t.\f.t;
    if    = \g.\t.\f. g t f
in if true 4 5
