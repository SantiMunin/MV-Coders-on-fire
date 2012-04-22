-module(decimal_to_roman).
-export([dec_to_roman/1]).

% dec_to_roman/1 : integer -> string
dec_to_roman(Num) when Num >= 1, Num =< 3999 ->
    dec_to_roman(Num, "",
        [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1],
        ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX",
         "V", "IV", "I"]);
dec_to_roman(_) ->
	io:format('Wrong number, please introduce a value between 1 and 3999.\n').

% dec_to_roman/4 : integer string (number list) (string list) -> string
%   Decimal to roman converter.
dec_to_roman(0, Acc, _Decimal, _Roman) -> Acc;
dec_to_roman(Num, Acc, Decimal, Roman) ->
	Dec_head = hd(Decimal),
	if Dec_head =< Num -> dec_to_roman(Num - Dec_head, Acc ++ hd(Roman), Decimal, Roman);
	true -> dec_to_roman(Num, Acc, tl(Decimal), tl(Roman))
	end.	
