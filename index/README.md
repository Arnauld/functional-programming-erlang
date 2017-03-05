```erlang
→ erl
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
1> c(index).
{ok,index}
2> eunit:test(index).
  All 15 tests passed.
ok
3> index:index_file("gettysburg-address.txt").
Index: [{"earth",[{28,28}]},
        {"shall",[{25,26},{28,28}]},
        {"altogether",[{10,10}]},
        {"proper",[{10,10}]},
        {"sense",[{13,13}]},
        {"consecrate",[{14,14}]},
        {"ground",[{14,14}]},
        {"dead",[{15,15},{25,25},{22,22}]},
        ...
```