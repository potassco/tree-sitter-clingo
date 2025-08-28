#minimize{ 1@"s":a }.
 % <- keyword.directive
 %         ^ number
 %          ^ punctuation.delimiter
 %            ^ string
 %               ^ function.call

#minimise{a,"\n",@f}.
 % <- keyword.directive
 %        ^ constant.macro
 %           ^ string.escape
 %               ^ function.macro
 %                ^ function.macro

#maximise{f(X,_)}.
 % <- keyword.directive
 %        ^ constructor
 %          ^ variable
 %            ^ variable.builtin

#edge (a,b).
 % <- keyword.directive
#heuristic a. [true,blub]
 % <- keyword.directive
#project a/3.
 % <- keyword.directive
#project a.
 % <- keyword.directive
#show.
 % <- keyword.directive
#show a/3.
 % <- keyword.directive
#show a : a.
 % <- keyword.directive
#defined a/3.
 % <- keyword.directive
 

#include "bar.lp".
 % <- keyword.import
 %         ^ string
#include <bar>.
 %         ^ module

not a.
 % <- keyword.operator
not not a.
   % <- keyword.operator
#sum{1:a}.
 % <- @keyword.operator
#sum+{1:a}.
 % <- @keyword.operator
#min{1:a}.
 % <- @keyword.operator
#max{1:a}.
 % <- @keyword.operator
#count{1:a}.
 % <- @keyword.operator

#external a.
 % <- keyword.modifier

#const c = 3.
 % <- keyword.directive.define
 %     ^ constant.macro
 %       ^ operator

&lp{a, b(1)}.
% <- keyword
% ^ keyword
%   ^ constant.macro
%      ^ constructor

#theory lp{
 % <- keyword
 %      ^ keyword
    lin_term {
    % <- type
     + : 0, binary, left;
     % <- operator
     %         ^ keyword
     %            ^ punctuation.delimiter
     %                ^ keyword
     + : 1, binary, right;
     %               ^ keyword	
     + : 2, unary
     %       ^ keyword
    };
    bounds{ };
    % <- type
    &op/0 :    lin_term, any;
    % <- keyword
    %  ^ punctuation.delimiter
    %   ^ number
                 % <- type
    %                    ^ keyword
    &op/1   : lin_term, { <=, >=, <, >, !=, = }, bounds, any
    %           ^ type
    %                     ^ operator
    %                                               ^ type
    %                                                  ^ punctuation.delimiter
    %                                                      ^ keyword
}.

#program acid(a).
 % <- keyword
 %        ^ module
 %            ^ constant.macro

#script (python)
 % <- keyword
 %         ^ keyword
#end.

#true. #false.
% ^ boolean
%        ^ boolean
