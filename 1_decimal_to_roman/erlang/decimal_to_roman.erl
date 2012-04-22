-module(decimal_to_roman).
-export([dec_to_roman/1]).

% dec_to_roman/1 : integer -> string
%   Map between decimal and romans
dec_to_roman(Num) ->
    dec_to_roman(Num, "",
        [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1],
        ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
         "V", "IV", "I"]).

% dec_to_roman/4 : integer string number (list string) -> string
%   Decimal to roman converter.
dec_to_roman(0, Acc, _Decimal, _Roman) -> Acc;
dec_to_roman(Num, Acc, Decimal, Roman) ->
	dec_head = hd(Decimal),
	if dec_head =< Num -> dec_to_roman(Num - dec_head, Acc ++ hd(Roman), Decimal, Roman);
	true -> dec_to_roman(Num, Acc, tl(Decimal), tl(Roman))
	end.	
