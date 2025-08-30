#minimize{ 1@"s":a }.
%  ^ keyword.directive
%          ^ number
%           ^ punctuation.delimiter
%             ^ string
%                ^ function.call

#minimise{a,"\n",@f}.
%  ^ keyword.directive
%         ^ constant.macro
%            ^ string.escape
%                ^ constant.macro
%                 ^ constant.macro

#maximise{f(X,_)}.
%  ^ keyword.directive
%         ^ constructor
%          ^ punctuation.bracket
%              ^ punctuation.bracket
%           ^ variable
%             ^ variable.builtin

:~ b. [1@3]
%^ punctuation.delimiter
%     ^ punctuation.bracket
%         ^ punctuation.bracket

#edge (a,b).
% ^ keyword.directive
%       ^ punctuation.delimiter
%          ^ punctuation.delimiter

#heuristic a. [true(1),blub]
% ^ keyword.directive
%          ^ function.call
%                ^ constructor
%                       ^ constant.macro

#project a/3.
% ^ keyword.directive
%        ^ function.call

#project a.
% ^ keyword.directive
%        ^ function.call

#show.
% ^ keyword.directive

#show -a/3.
% ^ keyword.directive

#show a : a.
%  ^ keyword.directive
%       ^ punctuation.delimiter

#defined a/3.
%  ^ keyword.directive
 

#include "bar.lp".
% ^ keyword.import
%          ^ string

#include <bar>.
%          ^ module

not a.
% ^ keyword.operator
%   ^ function.call

not not a.
%  ^  keyword.operator
%       ^ function.call

#sum{1:a}.
% ^ keyword.operator
%   ^ punctuation.bracket
%       ^ punctuation.bracket

#sum+{1:a}.
% ^ keyword.operator

#min{1:a}.
% ^ keyword.operator

#max{1:a}.
% ^ keyword.operator

#count{1:a}.
% ^ keyword.operator

#external a.
%  ^ keyword.directive

#const c = 3. [default]
%  ^ keyword.directive.define
%      ^ constant.macro
%        ^ operator
%                 ^ keyword

&lp{a, b(1,2)}.
% <- keyword
% ^ keyword
%   ^ constant.macro
%      ^ constructor

#theory lp{
% ^ keyword
%       ^ keyword
    lin_term {
%     ^ type
     + : 0, binary, left;
%    ^ operator
%              ^ keyword
%                 ^ punctuation.delimiter
%                     ^ keyword
     + : 1, binary, right;
%                    ^ keyword	
     + : 2, unary
%            ^ keyword
    };
    bounds{ };
%     ^ type
    &op/0 :    lin_term, any;
%     ^ keyword
%      ^ punctuation.delimiter
%       ^ number
%                  ^ type
%                        ^ keyword
    &op/1   : lin_term, { <=, >=, <, >, !=, = }, bounds, any
%               ^ type
%                         ^ operator
%                                                   ^ type
%                                                      ^ punctuation.delimiter
%                                                          ^ keyword
}.

#program acid(a).
%  ^ keyword
%         ^ module
%             ^ constant.macro

#script (python)
% <- keyword
%          ^ keyword
#end.
% ^ keyword

#true. #false.
% ^ boolean
%        ^ boolean

-a().
% <- function
%^ function
% ^ punctuation.bracket
%  ^ punctuation.bracket

p(f(),(),(,)).
% <- function
%  ^ punctuation.bracket
%   ^ punctuation.bracket
%     ^ punctuation.bracket
%      ^ punctuation.bracket
%         ^ punctuation.delimiter

a; not b.
% <- function
%^ punctuation.delimiter
%      ^ function.call

a: b | not a.
% <- function
%^ punctuation.delimiter
%  ^ function.call
%    ^ punctuation.delimiter
%          ^ function.call

#max{ 1:a:b; 2:not a: c }.
%       ^ function
%         ^ function.call
%                  ^ function.call

{ a }.
% ^ function

{ not a }.
%     ^ function.call

#external a : b.
%         ^function
%             ^function.call

:- a, b: c.
% <- punctuation.delimiter
%  ^ function.call
%     ^ function.call
%        ^ function.call

:- {a: b}.
%   ^ function.call
%      ^ function.call

p(1..1, 1^1, 1?1, 1+1, 1*1, 1\1, 1**1).
%  ^ operator
%        ^ operator
%             ^ operator
%                  ^ operator
%                       ^ operator
%                            ^ operator
%                                 ^ operator

p(~1, -1, |1|)
% ^ operator
%     ^ operator
%         ^ operator
%           ^ operator

1 < 1, 1 > 1, 1 <= 1, 1 >= 1, 1 = 1, 1 != 1.
% ^ operator
%        ^ operator
%               ^ operator
%                       ^ operator
%                               ^ operator
%                                       ^ operator
