
function asc=dpc2asc(dpc)
% function asc=dpc2asc(dpc)
% Converts CDC display code to ascii -
% each 3 bytes (24 bits) represent 4 characters
%
% 
% Author: Tihomir Hristov 
% 	  Atmospheric Turbulence Lab
% 	  University of California, Irvine
%         Tel: (714)-824-7437
% 	  tihomir@cafws2.eng.uci.edu 

        %
        % CDC display code equivalent of the ASCII table
        %
        t= ':ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/()$= ,.#[]%"_!&''?<>@\^;';

        w = 256 .^ [2 1 0];

        dpc = dpc(:)';
        ndpc = max(size(dpc));

        R = rem(ndpc, 3);
        if (R ~=0 )
                dpc = [dpc zeros(1, 3-R)];
        end

        ndpc = prod(size(dpc));
        nasc = ndpc * 8 / 6 ;

        asc = zeros(4, nasc/4);
        tmp = w * reshape(dpc, 3, length(dpc) / 3);
        for j=4:-1:1
                asc(j,:) = t(rem(tmp, 64) + 1) ;
                tmp = fix(tmp / 64);
        end
        asc = asc(:);
