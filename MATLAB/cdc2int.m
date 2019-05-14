function  intbuf=cdc2int(cdcbuf)
%
% function  intbuf=cdc2int(cdcbuf)
% converts 20 bit words to integers
% For matrices operates columnwise
%
% 
% Author: Tihomir Hristov 
% 	  Atmospheric Turbulence Lab
% 	  University of California, Irvine
%         Tel: (714)-824-7437
% 	  tihomir@cafws2.eng.uci.edu 

        w = 256 .^ [4 3 2 1 0];

        [nr nc] = size(cdcbuf);

        if(nr == 1) % if a row vector
                cdcbuf = cdcbuf(:);
        end

        R = rem(nr, 5);
        if (R ~=0 )
                cdcbuf = [cdcbuf;  zeros(5-R, nc)];
        end
        [nr nc] = size(cdcbuf);
        nrint = nr * 2 / 5;

        cdcbuf = cdcbuf(:);
        ncdc = max(size(cdcbuf));

        nint = ncdc * 2 / 5 ;

        intbuf = zeros(2, nint/2);
        tmp = w * reshape(cdcbuf, 5, length(cdcbuf) / 5);
        for j=2:-1:1
                intbuf(j,:) = rem(tmp, 2^20) ;
                tmp = fix(tmp / 2^20);
        end
        intbuf = reshape(intbuf, nrint, nc);
