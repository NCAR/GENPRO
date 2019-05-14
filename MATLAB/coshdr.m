function col = coshdr(header, ColumnName)
%
% Interprets the header and extacts the column selected
% by its name. The possible names are
%
%          VarNo        - column #1
%          ScanRate     - column #2
%          Comment      - column #3
%          VarName      - column #4
%          Units        - column #5
%          Divisor      - column #6
%          Intercept    - column #7
%
% 
% Author: Tihomir Hristov 
% 	  Atmospheric Turbulence Lab
% 	  University of California, Irvine
%         Tel: (714)-824-7437
% 	  tihomir@cafws2.eng.uci.edu 



ColNames = [ 'VarNo    '
             'ScanRate '
             'Comment  '
             'VarName  '
             'Units    '
             'Divisor  '
             'Intercept' ];

NTextRows = 11; % 11 lines of text before the channel table
nvars = size(header, 1) -  NTextRows ;

boc = [1  7  14  56  66  80  91 ] ;
eoc = [3  8  55  65  74  86  97 ] ;

hr = size(header, 1); % Header rows
hc = size(header, 2); % Header columns

ColumnName = ColumnName(ColumnName > 32) ; % remove spaces and controls

ColNo = listsrc(ColNames, ColumnName) ;

col = header(NTextRows + (1:nvars), boc(ColNo):eoc(ColNo)) ;  

if (find(ColNo == [1 2 6 7]) ~= [])
        col = str2num(col) ;  
end
