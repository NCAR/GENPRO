function [index, fulllist, OneFound]=listsrc(list, strings2find)
%
%       function [index, fulllist, OneFound]=listsrc(list, strings2find)
%
%       The function returns a vector containing the indices in the list
%       of the strings specified in the strings2find variable and a list
%       of the items found. Wildcards are accepted.
%
%       list         - a string matrix, each row is a string
%
%       strings2find - a string matrix containing the strings
%                      to search for. Wildcards are accepted ---
%                      e.g. if strings2find = 'app*' then
%                      all the strings  in the 'list' variable
%                      starting with 'app' will be selected and
%                      their indices will appear in 'index'.
%
%       index        - an array containing the indices of the
%                      strings found in 'list'
%
%       fulllist     - a text variable containing all the 'list' items
%                      matching the wildcars in the 'list' variable (i.e.
%                      all the '*' and '?' are expanded).
%
%       Example:
%       list = ['apple    '
%               'banana   '
%               'pineapple'
%               'orange   '];
%
%       strings2find = ['banana'
%                       'orange'];
%
%       index = [2 4];
%
%         
% Author: Tihomir Hristov 
% 	  Atmospheric Turbulence Lab
% 	  University of California, Irvine
%         Tel: (714)-824-7437
% 	  tihomir@cafws2.eng.uci.edu 


%
% Convert to upper case; the search is case insensitive
%
strings2find((strings2find >= 'a') & (strings2find <= 'z')) =  ...
           setstr(strings2find((strings2find >= 'a') & (strings2find <= 'z')) + 'A' - 'a');

list((list >= 'a') & (list <= 'z')) =  ...
           setstr(list((list >= 'a') & (list <= 'z')) + 'A' - 'a');

[nrows ncols]=size(strings2find);

index = [];

%
% now start a linear search in the list for the strings2find items
%
OneFound = 0;
for i=1:nrows
        item = strings2find(i,:) ;
        item = item(item > 32);  % remove spaces

        %
        % check for asterisks '*' and question marks '?'
        %
        ast_flag = any(item == '*');
        if(ast_flag)
                % Remove everything after the asterisk
                item(min(find(item == '*')) + 1:length(item)) = [];
        end
        ast_index = max(find(item ~= '*'));
        if(ast_index == [])
                %
                % if asterisk only, then all the items are selected
                %

                index = 1:size(list, 1);
                found = 1;
                OneFound = OneFound | found;
                break ;
        end

        q_flag = any(item == '?');
        q_index = find((item(1:ast_index) ~= '?'));

        found = 0; % the strings2find item not found yet
        for j=1:size(list,1)
                ListItem=list(j,:) ;
                ListItem = ListItem(ListItem > 32) ; % remove spaces

                Chars2Compare = 1:ast_index ;
                Chars2Compare = Chars2Compare(q_index);  % exclude question marks from being compared
              % Chars2Compare = Chars2Compare(Chars2Compare <= max(size(ListItem)));

                if(ast_index <= max(size(ListItem)))
                        % [item(Chars2Compare) ' ' ListItem(Chars2Compare) ' '  list(j,:) ]
                        if all(item(Chars2Compare) == ListItem(Chars2Compare))
                                % item found
                                index = [index j];
                                found = 1;
                        end
                end
        end

        if ~found
                disp(['There is no sensor matching the description ' strings2find(i,:)]) ;
        end

        OneFound = OneFound | found;
end

%
% Now let's remove the duplicated indices
%
nindex = max(size(index));
staying = ones(size(index));

while(i < nindex)
        if(staying(i) == 1)
                tmp = find(~(index - index(i)));
                staying(tmp(2:max(size(tmp)))) = zeros(1, max(size(tmp))-1);
        end
        i = i + 1;
end

index=index(staying) ;

if (nargout > 1)
     fulllist = list(index,:);
end
