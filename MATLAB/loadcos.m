function [header, DataFast, fclFast, DataSlow, fclSlow]=loadcos(filename, varlist)
%
% function [header, DataFast, fclFast, DataSlow, fclSlow]=loadcos(filename, varlist)
%
% Reads the selected channels from a COS-blocked CODE-I/CODE-II file.
%
% Input variables:
%
%     filename - the full pathname of the file to load
%
%     varlist  - a text variable containing the names of the selected
%                channels.  Wildcards including asterisks '*' and
%                question marks '?' are accepted, e.g. if varlist = 'al*'
%                then all the channels starting with 'al' are selected;
%
% Output variables:
%
%     header   - file header is returned as a 100 column text matrix;
%                to get the file header only, call the function as follows:
%
%                           header = loadcos(filename) ;
%
%     DataFast - a matrix containing all the 20 Hz channels; each
%                channel is a row, each scan is a column
%
%     fclFast  - a text matrix containing the list of the names of all
%                20 Hz channels; the order of names matches the order
%                of the channels in the DataFast matrix
%
%     DataSlow - a matrix containing all the 1 Hz channels; each
%                channel is a row, each scan is a column
%
%     fclSlow  - a text matrix containing the list of the names of all
%                1 Hz channels; the order of names matches the order
%                of the channels in the DataSlow matrix
% 
%     Use listsrc(fclFast, ChannelNames)  to find the index of channels 
%     in  DataFast or listsrc(fclSlow, ChannelNames) to find them in 
%     DataSlow.  
%
% Example:
% [h d20 fcl20 d1 fcl1]=loadcos('g50453', ['time '; 'hgm  '; 'alat '; 'along'; 'hp   ']);
% plot3(d20(3,:), d20(2,:), d20(1,:), d20(3,:), d20(2,:), d20(4,:)); grid
% 
% Author: Tihomir Hristov 
% 	  Atmospheric Turbulence Lab
% 	  University of California, Irvine
%         Tel: (714)-824-7437
% 	  tihomir@cafws2.eng.uci.edu 

%
% Check if the file exists and open it.
%
if (exist(filename) == 0)
        error(['File ' filename ' does not exist.']);
end

fd = fopen(filename, 'rb');

if(fd == -1)
        error(['File ' filename ' was not open.']);
end

%
% Now, let's get the filesize
%
  fseek(fd, 0, 'eof') ;
  filesize = ftell(fd) ;
  fseek(fd, 0, 'bof') ;


%
% Let's read the first several (5) blocks, where the header should be...
%
BlockSizeBytes    = 4096 ;
BlockSizeWords    =  512 ;
WordSizeBits      =   64 ; % bits
WordSizeBytes     =    8 ; % bytes
DataWordsPerBlock =  511 ;
BitsPerNumber     =   20 ;
NTextRows         =   11 ;
FastRate          =   20 ; % Hz
SlowRate          =    1 ; % Hz

NBlocks = 5;

buf = fread(fd, NBlocks * BlockSizeBytes, 'uchar');

%
% Get the control words
%
CtrlWordsIndex = [];
for i=1:NBlocks
        CtrlWordsIndex= [ CtrlWordsIndex  ((1:WordSizeBytes) + (i-1)*BlockSizeBytes)];
end
CtrlWordsBytes = reshape(buf(CtrlWordsIndex), WordSizeBytes, NBlocks)'; % Each row is a word
buf(CtrlWordsIndex) = [];
CtrlWordsIndex      = []; % Remove the control words

% The control word has 64 bits
% Their meaning is as follows (the most significant bit has number 0,
% the least significant - 63):
%
%   bits 0 to 3   (4 bits)  -- M    = control word type
%   bit  11       (1 bit)   -- NBDF = bad data flag
%   bits 31 to 54 (24 bits) -- NBN  = cray block number
%          NBN=rem(floor( ((256 .^[3 2 1 0]) * CtrlWordsBytes(:,5:8)')' / 512), 2^24);  % or
%          NBN=rem(floor( ((256 .^[  2 1 0]) * CtrlWordsBytes(:,5:7)')' /   2), 2^24);
%   bits 55 to 63 (9 bits)  -- NFWI = forward word indicator (pointer to the next control word)

%
% forward word indicator (pointer to the next control word)
%
fwi = rem(CtrlWordsBytes(:,7) * 256 + CtrlWordsBytes(:,8), 512) ;
NHeaderBlocks = min(find(fwi ~= 511)); % Number of header blocks
HeaderSizeBytes = WordSizeBytes * (sum(fwi(1:NHeaderBlocks))) ;                 % Buffer size
HeaderLength    = WordSizeBytes * (sum(fwi(1:NHeaderBlocks)) + NHeaderBlocks);  % length in the file or
                                                                                % Data start address
%
% Convert the file header from CDC display code to ASCII
%
header = dpc2asc(buf(1:HeaderSizeBytes));
header = reshape(header(1:100 * fix(length(header) / 100)), 100, fix(length(header) / 100))' ;
NVars = size(header, 1) - NTextRows;  ;    % Number of variables described in the header

if (nargout == 1)
        % Only the file header is expected as an output
        return;
end

Divisors   = coshdr(header, 'Divisor');
Intercepts = coshdr(header, 'Intercept');

ScanRates  = coshdr(header, 'ScanRate');
RecordSizeWords = 1 + ceil((BitsPerNumber * sum(ScanRates)) / WordSizeBits);
RecordSizeBytes = WordSizeBytes * RecordSizeWords;

NRecords = floor(  (filesize - WordSizeBytes * (filesize / BlockSizeBytes) - HeaderLength) / RecordSizeBytes) ;

%
% Variable Extraction Mask
%
eov = cumsum(ScanRates);             % End of variable
bov = 1 + [0; eov(1:(NVars - 1))];   % Beginning of variable

% bov_byteindex = floor((bov - 1) * 5 / 2 ) + 1;
% eov_byteindex = ceil(bov_byteindex + 1.5 );
%
% bov_5bi = floor((ElementNo - 1) / 2) * 5 + 1 ; % + (0:4)
%

VarNames = coshdr(header, 'VarName');
VarIndex = listsrc(VarNames, varlist);
VarRates = ScanRates(VarIndex);
VarNames = VarNames(VarIndex,:);

NFast = sum(VarRates == FastRate) ;
NSlow = sum(VarRates == SlowRate) ;

if (any((VarRates ~= FastRate) & (VarRates ~= SlowRate)))
        error('Sampling rates different from 1 or 20 Hz') ;
end

% Total number of samples in the file
NSamples = NRecords * (FastRate * NFast + SlowRate * NSlow) ;

% Fast (20Hz) channels selected
fclFast  = VarNames(find(VarRates == FastRate), :) ;

% Slow ( 1Hz) channels selected
fclSlow  = VarNames(find(VarRates == SlowRate), :) ;

VarIndexFast  =  VarIndex(find(VarRates == FastRate)) ;
VarIndexSlow  =  VarIndex(find(VarRates == SlowRate)) ;

DivFast = Divisors(VarIndexFast);
DivSlow = Divisors(VarIndexSlow);

IntFast = Intercepts(VarIndexFast);
IntSlow = Intercepts(VarIndexSlow);

%
% Alocate memory
%
DataFast = zeros(NFast, FastRate * NRecords) ;
DataSlow = zeros(NSlow, SlowRate * NRecords) ;

DataStartBytes = HeaderLength ;
fseek(fd, DataStartBytes, 'bof') ;
RecordsToRead = 50; % Number of records to read at a atime
RecordsRead = 0;

FastPtr = 0;
SlowPtr = 0;


 while(RecordsRead < NRecords)
        WordsToNextBlock = (BlockSizeBytes * ceil(DataStartBytes / BlockSizeBytes) - DataStartBytes) / WordSizeBytes ;
        NBlockCtrlWords = (RecordsToRead * RecordSizeWords - WordsToNextBlock > 0) * ...
                                (1 + floor((RecordsToRead * RecordSizeWords - WordsToNextBlock) / (DataWordsPerBlock))) ;
        WordsToRead = RecordsToRead * RecordSizeWords + NBlockCtrlWords ;

        %
        % Pointer to the first byte (not to all 8) of the block control word in the buffer
        %
        BlockCtrlWordsIndex = (WordSizeBytes*WordsToNextBlock) + 1 + BlockSizeBytes * (0:NBlockCtrlWords-1) ;

        %
        % To eliminate block control words from the buffer; Then eliminate record control words
        %
        BlockCtrlWordsIndex = ([0:7]' * ones(size(BlockCtrlWordsIndex))) + BlockCtrlWordsIndex(ones(8,1), :)  ;
        BlockCtrlWordsIndex = BlockCtrlWordsIndex(:) ;

        buf = fread(fd, WordSizeBytes * WordsToRead, 'uchar');
        BlockCtrlWords = buf(BlockCtrlWordsIndex) ;
        buf(BlockCtrlWordsIndex) = [] ; % Block Control words eliminated

        buf = reshape(buf, RecordSizeBytes, max(size(buf)) / RecordSizeBytes) ; % Each record is a column
        Record = cdc2int(buf(9:RecordSizeBytes, :)) ;

        if ( VarIndexFast ~= [])
                for NFastVar = 1:max(size(VarIndexFast))
                        tmp = Record(bov(VarIndexFast(NFastVar)):eov(VarIndexFast(NFastVar)),  :) ;
                        DataFast(NFastVar, FastPtr + (1:(RecordsToRead * FastRate))) = tmp(:)';
                end
                FastPtr = FastPtr + RecordsToRead * FastRate ;
        end

        if ( VarIndexSlow ~= [])
                for NSlowVar = 1:max(size(VarIndexSlow))
                        tmp = Record(bov(VarIndexSlow(NSlowVar)):eov(VarIndexSlow(NSlowVar)),  :);
                        DataSlow(NSlowVar, SlowPtr + (1:RecordsToRead)) =  tmp(:)';
                end
                SlowPtr = SlowPtr + RecordsToRead * SlowRate ;
        end

        %
        % Main loop epilogue
        %
        DataStartBytes = DataStartBytes + WordSizeBytes * WordsToRead ;
        RecordsRead = RecordsRead + RecordsToRead;
        RecordsToRead = min(NRecords - RecordsRead,   RecordsToRead);
        disp(['Records read ' int2str(RecordsRead) ' of ' int2str(NRecords)])
end

%
% Calibrate the data
%
if ( VarIndexFast ~= [])
         for NFastVar = 1:max(size(VarIndexFast))
                 DataFast(NFastVar, :) = DataFast(NFastVar, :) / DivFast(NFastVar) - IntFast(NFastVar);
         end
end

if ( VarIndexSlow ~= [])
         for NSlowVar = 1:max(size(VarIndexSlow))
                 DataSlow(NSlowVar, :) = DataSlow(NSlowVar, :) / DivSlow(NSlowVar) - IntSlow(NSlowVar);
         end
end

fclose(fd);
