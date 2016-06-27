function gen2nc( gendata, varargin )
% GEN2NC - Convert GENPRO King Air data files to netCDF
%
% Usage:
%   gen2nc( gendata, [key, value] )
%      gendata - name of the GENPRO data file
%      key, value - option name and value
% Options
%   timezone, hours  - times are 'hours' behind UTC
%     for example for MST: 'timezone', -7
%
% Notes:
%   Opens two files: gendata_hd for the header information
%     and gendata for the data.
%   1DC counts per bin are calculated from concentrations per bin
%     assuming the sample area for each bin size is:
%     0.024,0.225,0.692,1.313,1.944,2.643,3.386,4.147,4.898,
%     5.616,6.272,6.842,7.300,6.588,5.856/
%   2D particle sizes are calculated from the maximum dimension along the 
%     flight track
%   2D concentrations are calculated from 2D counts per bin assuming a sample
%     area enhancement of (32+n)/32
% 
% History:
%   written: Larry Oolman, UWYO, September 2012
%

if nargin < 1
% cprintf('error','Not enough input arguments.\n\n')
  disp('Not enough input arguments.')
  disp(' ')
  help gen2nc
  return
end

if mod(length(varargin),2)
% cprintf('error','Optional arguments must be in pairs.\n\n');
  disp('Optional arguments must be in pairs.');
  disp(' ')
  help gen2nc
  return
end

setenv('TZ','UTC0');
[vars,gmeta] = readhdr( gendata );
if isempty(vars), return, end

% Set options
gmeta.timeoff = 0;
for i = 1:2:length(varargin)
  switch varargin{i}
  case 'timezone'
    gmeta.timeoff = varargin{i+1}/24;
    gmeta.starttm = gmeta.starttm - gmeta.timeoff;
    gmeta.endtm   = gmeta.endtm   - gmeta.timeoff;
  otherwise
%   cprintf('error',[ 'Unknown option: ' varargin{i} ]);
    disp([ 'Unknown option: ' varargin{i} ]);
    help gen2nc
    return
%   gmeta.(varargin{i}) = varargin{i+1};
  end
end

data = readdata( gendata, vars, gmeta );
if isempty(vars), return, end

nc = createnc( gendata, vars, gmeta);
writenc( nc, vars, gmeta, data );
netcdf.close(nc);

%-----------------------------------------------------------
function [vars,gmeta] = readhdr( gendata )
% Open the header
hdrid = fopen( [ gendata '_hd' ] );
if hdrid < 0,
  disp(['Unable to open GENPRO header file: ' gendata '_hd']);
  return;
end

vars = [];
col  = 0;
% Read each line generating a cell array of structs of the attributes
while 1
  tline = fgetl(hdrid);
  if ~ischar(tline), break, end
  if length(tline) < 8; continue; end
  if strncmpi(tline,'/PROJECT',8),
    i = strfind(tline,'"');
    gmeta.project = qstring(tline,1);
    disp(['project="' gmeta.project '"']);
  elseif strncmpi(tline,'/PRDATE',7),
    dstr = [ qstring(tline,1) '-' qstring(tline,2) '-' qstring(tline,3) ];
  elseif strncmpi(tline,'/PRTIME',7),
    h = qstring(tline,1); h = h(1:2);
    m = qstring(tline,2); m = m(1:2);
    s = qstring(tline,3); s = s(1:2);
    gmeta.starttm = datenum([ dstr ' ' h ':' m ':' s ]);
  elseif strncmpi(tline,'/ENDSNP',7)
    tvec = datevec(gmeta.starttm);
    hms = textscan(tline,'/ENDSNP = ( %f , %f , %f');
    tvec(4) = hms{1};
    tvec(5) = hms{2};
    tvec(6) = hms{3};
    gmeta.endtm = datenum(tvec);
    if gmeta.endtm < gmeta.starttm, gmeta.endtm = gmeta.endtm + 1, end
    dt = gmeta.endtm - gmeta.starttm;
  elseif strncmpi(tline,'/EXDATE',7),
    dstr = [ qstring(tline,1) '-' qstring(tline,2) '-' qstring(tline,3) ];
  elseif strncmpi(tline,'/EXTIME',7),
    h = qstring(tline,1); h = h(1:2);
    m = qstring(tline,2); m = m(1:2);
    s = qstring(tline,3); s = s(1:2);
    gmeta.prtime = datenum([ dstr ' ' h ':' m ':' s ]);
  elseif strncmpi(tline,' APPVAR',7),
    i = strfind(tline,'=');
    names = csv(tline(i(1)+1:end));
    for i = 1:length(names)
      nm = names{i};
      col = col + 1;
      if regexp(nm,'[A-Za-z][0-9][0-9]$')
        nv = str2num(nm(end-1:end));
        if nv == 1, 
          vars(end+1).name = nm(1:end-2);
          vars(end).col = col;
          disp([num2str(vars(end).col) ' ' vars(end).name ' is a vector variable.'])
        end
        vars(end).vecsz = nv;
      else
        vars(end+1).name = names{i};
        vars(end).vecsz = 1;
        vars(end).col = col;
      end
    end
  elseif strncmpi(tline,' ORDVAR',7),
    i = strfind(tline,'=');
    ords = csv(tline(i(1)+1:end));
    nval = 0;
    vec  = 0;
  elseif strncmpi(tline,' LETVAR',7),
    i = strfind(tline,'=');
    j = strfind(tline,'%FOR');
    if isempty(j), j=length(tline)+1; end
    attrs = csv(tline(i(1)+1:j(1)-1));
    if vec == 0
      nval = nval + 1;
      vec = vars(nval).vecsz - 1;
    else
      vec = vec - 1;
    end
    for i = 1:length(ords)
      a = attrs{i};
      if a(1:1) == '"'
        a = qstring(a,1);
% Clean up titles that have vector element specific information
        if strcmp(ords{i},'TITLE')
          if vars(nval).vecsz > 1
% Read in the CellSizes
            sz = regexp(a,'ice <= ([0-9]+)','tokens');
            if ~isempty(sz); 
              s = str2num(sz{1}{1});
              vars(nval).CellSizes = [ s/2 s ];
            end
            sz = regexp(a,'ice [0-9]+-([0-9]+)','tokens');
            if ~isempty(sz); 
              vars(nval).CellSizes(end+1) = str2num(sz{1}{1});
            end
            sz = regexp(a,'ice > ([0-9]+)','tokens');
            if ~isempty(sz); 
              cs = vars(nval).CellSizes;
              vars(nval).CellSizes(end+1) = 2*cs(end) - cs(end-1);
            end

% Now simplify the TITLE
            ii = regexp(a,' *- *chan');
            if ~isempty(ii); a = a(1:ii(1)-1); end
            ii = regexp(a,'concentration -');
            if ~isempty(ii); a = [a(1:ii(1)-1) 'concentration']; end
            ii = regexp(a,'concentration *all');
            if ~isempty(ii); a = [a(1:ii(1)-1) 'cumulative concentration']; end
            ii = regexp(a,'concentration *[>=]');
            if ~isempty(ii); a = [a(1:ii(1)-1) 'cumulative concentration']; end
            ii = regexp(a,'ice [<>0-9]');
            if ~isempty(ii); a = [a(1:ii(1)-1) 'counts per bin']; end
            ii = regexp(a,' >= [0-9]* um');
            if ~isempty(ii); a = [ a(1:ii(1)-1) '']; end
            ii = regexp(a,' [0-9]');
            if ~isempty(ii); a = a(1:ii(1)-1); end
          end
        end
        vars(nval).(ords{i}) = a;
      else
        vars(nval).(ords{i}) = str2num(a);
      end
    end
  else
    str = 'AIRCRAFT POSITION DATA RELATIVE TO';
    i = strfind(tline,str);
    if ~isempty(i)
      tline = tline(i(1)+length(str):end);
      j = strfind(tline,'"');
      ctrname = strtrim(tline(1:j(1)-1));
    end
    str = 'LATITUDE:';
    i = strfind(tline,str);
    if ~isempty(i)
      ctrlat = textscan(tline(i(1)+length(str):end),'%f');
      ctrlat = ctrlat{1};
    end
    str = 'LONGITUDE:';
    i = strfind(tline,str);
    if ~isempty(i)
      ctrlon = textscan(tline(i(1)+length(str):end),'%f');
      ctrlon = ctrlon{1};
    end
  end
end
fclose(hdrid);
for i = 1:length(vars)
  switch vars(i).name;
  case 'date',
    vars(i).name = 'DATE';
  case 'time'
    vars(i).name = 'TIME';
  case 'fssp'
    if vars(i).vecsz == 1; 
      vars(i).name = pmsvar(vars(i).name);
    else
      vars(i).name = 'oldfssp';
    end
  case 'tas'
    vars(i).name = 'TASX';
  otherwise
    vars(i).name = pmsvar(vars(i).name);
  end
end
return

%-----------------------------------------------------------
function qval = qstring(sval,n)
% Extract the nth quoted string from sval
i = strfind(sval,'"');
if length(i) < 2*n,
  disp('qstring: Not enough quoted values.');
  disp(sval);
  qval = '';
  return
end
if i(2*n)-i(2*n-1) == 1;
  qval = '';
else
  qval = strtrim(sval(i(2*n-1)+1:i(2*n)-1));
end
return

%-----------------------------------------------------------
function tokens = csv(s)
tokens = [];
while length(s) > 0
  ic = strfind(s,',');
  iq = strfind(s,'"');
  if length(iq) >= 2
    if isempty(ic) | iq(1) < ic(1)
      t = s(iq(1):iq(2));
      ic = ic(ic>iq(2));
      if isempty(ic);
        s = [];
      else
        s = s(ic(1)+1:end);
      end
    else
      [t,s] = strtok(s,',');
    end
  else
    [t,s] = strtok(s,',');
  end
  tokens{end+1} = strtrim(t);
end
return

%-----------------------------------------------------------
function data = readdata( gendata, vars, gmeta )
fid = fopen( gendata );
vec = [ vars.vecsz ];
rate = [ vars.RATE ];
sz = sum( vec .* rate );
fprintf('There are %d values in each record\n',sz);
ntm = round((gmeta.endtm - gmeta.starttm) * 86400 + 1);
if vars(1).CONKEY == 0
  data = fread(fid,[sz,ntm],'single',0,'ieee-be');
  hms = data(2:sz:end);
  ii = find(diff(hms)~=1 & diff(hms)~=41 & diff(hms)~=4041);
  for iii = 1:length(ii)
    fprintf('Data gap: %d to %d.\n', hms(ii(iii):ii(iii)+1));
  end
else
  data = fread(fid,[sz,ntm],'uint32',0,'ieee-be');
end
sclkey = [ vars.SCLKEY ];

for ii = find(sclkey == 2)
  v  = vars(ii);
  jj = v.col:v.col+v.RATE*v.vecsz-1;
  data(jj,:) = data(jj,:) ./ v.FACTOR - v.TERM;
end
disp(sprintf('TIME spans %f to %f.',min(data(3,:)),max(data(3,:))))

%-----------------------------------------------------------
function nc = createnc( gendata, vars, gmeta )

timefmt = 'yyyy-mm-ddTHH:MM:SS +0000';

ncfile = [ gendata '.c1.nc' ];
nc     = netcdf.create(ncfile,0);

% Create dimensions
ntm = round((gmeta.endtm - gmeta.starttm) * 86400 + 1);
timeD = netcdf.defDim(nc,'time',ntm);
for sps = unique( [ vars.RATE ] )
  netcdf.defDim(nc,sprintf('sps%d',sps),sps);
end
for vec = unique( [ vars.vecsz ] )
  if vec > 1
    vec = vec + 1;
    netcdf.defDim(nc,sprintf('vec%d',vec),vec);
  end
end

% Create global attributes
g = netcdf.getConstant('NC_GLOBAL');
netcdf.putAtt(nc,g,'institution', ...
  'University of Wyoming, Department of Atmospheric Science');
netcdf.putAtt(nc,g,'Address', ...
  'Dept. 3038, 1000 E. University Ave., Laramie, WY 82071-3038');
netcdf.putAtt(nc,g,'Phone', '(307) 766-3246');
netcdf.putAtt(nc,g,'creator_url', 'http://www-das.uwyo.edu');
netcdf.putAtt(nc,g,'Conventions', 'NCAR-RAF/nimbus');
netcdf.putAtt(nc,g,'ConventionsURL', ...
  'http://www.eol.ucar.edu/raf/Software/netCDF.html');
netcdf.putAtt(nc,g,'ConventionsVersion', '1.3');
netcdf.putAtt(nc,g,'date_created', datestr(now,timefmt));
netcdf.putAtt(nc,g,'ProjectName', gmeta.project);
netcdf.putAtt(nc,g,'Platform', 'N2UW');
netcdf.putAtt(nc,g,'FlightDate', datestr(gmeta.starttm,'mm/dd/yyyy'));
netcdf.putAtt(nc,g,'TimeInterval', [ datestr(gmeta.starttm, 'HH:MM:SS'), ...
  datestr(gmeta.endtm,'-HH:MM:SS') ] );
netcdf.putAtt(nc,g,'time_coverage_start', datestr(gmeta.starttm,timefmt));
netcdf.putAtt(nc,g,'time_coverage_end',   datestr(gmeta.endtm,  timefmt));
if gmeta.timeoff
  netcdf.putAtt(nc,g,'comment',['Times adjusted by ' ...
    num2str(gmeta.timeoff*24) ' hours to make them UTC.' char(10) ...
    'TIME, DATE, and totalsec are still local time']);
end

names = strvcat(vars.name);
for nm = { 'latg','lath' }
  if strmatch(nm,names,'exact')
    lat = nm{1};
    netcdf.putAtt(nc,g,'latitude_coordinate', lat);
    netcdf.putAtt(nc,g,'geospatial_lat_min', single(-32767.));
    netcdf.putAtt(nc,g,'geospatial_lat_max', single(-32767.));
    break;
  end
end
for nm = { 'long','lonh' }
  if strmatch(nm,names,'exact')
    lon = nm{1};
    netcdf.putAtt(nc,g,'longitude_coordinate', lon);
    netcdf.putAtt(nc,g,'geospatial_lon_min', single(-32767.));
    netcdf.putAtt(nc,g,'geospatial_lon_max', single(-32767.));
    break;
  end
end
for nm = { 'altg','ztrue','z' }
  if strmatch(nm,names,'exact')
    alt = nm{1};
    netcdf.putAtt(nc,g,'zaxis_coordinate', alt);
    netcdf.putAtt(nc,g,'geospatial_vertical_min', single(-32767.));
    netcdf.putAtt(nc,g,'geospatial_vertical_max', single(-32767.));
    netcdf.putAtt(nc,g,'geospatial_vertical_positive', 'up');
    netcdf.putAtt(nc,g,'geospatial_vertical_units', 'm');
    break;
  end
end
if exist('lat','var') & exist('lon','var') & exist('alt','var')
  netcdf.putAtt(nc,g,'coordinates', ['time' ' ' lat ' ' lon ' ' alt ]);
end
for nm = { 'hwdir','hwdirh' }
  if strmatch(nm,names,'exact'); wd = nm{1}; break; end
end
for nm = { 'hwspd','hwmagh' }
  if strmatch(nm,names,'exact'); ws = nm{1}; break; end
end
for nm = { 'hw' }
  if strmatch(nm,names,'exact'); wv = nm{1}; break; end
end
if exist('wd','var') & exist('ws','var') & exist('wv','var')
  netcdf.putAtt(nc,g,'wind_field', [ws ' ' wd ' ' wv ]);
end

% Add variables
varid = netcdf.defVar(nc,'time','float',timeD);
netcdf.putAtt(nc,varid,'units', ...
  ['seconds since ' datestr(gmeta.starttm,'yyyy-mm-dd HH:MM:SS +0000')]);
netcdf.putAtt(nc,varid,'standard_name','time');
netcdf.putAtt(nc,varid,'strptime_format','seconds since %F %T %z');

for i = 1:length(vars)
  v = vars(i);
  if strcmp(v.name,'fsspold'), continue, end
% if strcmp(v.name,'A2DC_IBR'),   continue, end
% if strcmp(v.name,'A2DP_OBL'),   continue, end

% disp(['Defining ' v.name])
  switch v.name(1:min(3,length(v.name)))
   case 'lat'
    xtype = 'double';
  case 'lon'
    xtype = 'double';
  otherwise
    xtype = 'float';
  end
  spsID = netcdf.inqDimID(nc,sprintf('sps%d',v.RATE));
  if v.vecsz > 1
    dimids = [netcdf.inqDimID(nc,sprintf('vec%d',v.vecsz+1)) spsID timeD];
  elseif v.RATE > 1
    dimids = [spsID timeD];
  else
    dimids = timeD;
  end
  if strcmp(v.name,'fsspsa'), continue, end
  varid = netcdf.defVar(nc,v.name,xtype,dimids);
  if ~isempty(v.UNITS), netcdf.putAtt(nc,varid,'units',v.UNITS); end
  if ~isempty(v.TITLE), netcdf.putAtt(nc,varid,'long_name',v.TITLE); end
  switch v.name
  case {'latg','latg2','lath','long','long2','lonh'}
     netcdf.putAtt(nc,varid,'_FillValue',0.0);
  case {'altg','altg2','galt','galt2','gvew','gvew2','gvns','gvns2', ...
      'gvz','gvz2','gx','gx2','gy','gy2','mvar','svolc','svolp','toflla',...
      'toflla2','tofvel2' }
     netcdf.putAtt(nc,varid,'_FillValue',single(0));
  case {'matime','tofvel'}
    netcdf.putAtt(nc,varid,'_FillValue',single(-888));
  case {'huerr','hverr','hupdate','hxerr','hyerr','ralt2','xdist','ydist' }
    netcdf.putAtt(nc,varid,'_FillValue',single(-999));
  case {'AFSSP_IBL','CFSSP_IBL'}
    if strcmp(v.name,'CFSSP_IBL')
      netcdf.putAtt(nc,varid,'units','#/cm3');
    end
    netcdf.putAtt(nc,varid,'CellSizes', ...
      single([1.5:3:47.5,1:2:31,0.5:1:15.5,0.25:.5:7.75]));
    netcdf.putAtt(nc,varid,'FirstBin',int32(1));
    netcdf.putAtt(nc,varid,'LastBin',int32(15));
    netcdf.putAtt(nc,varid,'BeamDiameter',0.197);
    netcdf.putAtt(nc,varid,'BeamDiameterUnits','mm');
    netcdf.putAtt(nc,varid,'DepthOfField',2.8);
    netcdf.putAtt(nc,varid,'DepthOfFieldUnits','mm');
    netcdf.putAtt(nc,varid,'SampleArea',0.197 * 2.8);
    netcdf.putAtt(nc,varid,'SampleAreaUnits','mm2');
  case {'FACT_IBL','FBMFR_IBL'}
    netcdf.putAtt(nc,varid,'units','fraction');
  case {'C200X_OBR'}
    netcdf.putAtt(nc,varid,'CellSizes', single([6.25:12.5:193.75]));
    netcdf.putAtt(nc,varid,'FirstBin',int32(3));
    netcdf.putAtt(nc,varid,'LastBin',int32(15));
    avarid = netcdf.defVar(nc,[ 'A' v.name(2:end) ],xtype,dimids);
    netcdf.putAtt(nc,avarid,'long_name', '1DC counts per bin');
    netcdf.putAtt(nc,avarid,'units', '#');
    netcdf.putAtt(nc,avarid,'CellSizes', single([6.25:12.5:193.75]));
    netcdf.putAtt(nc,avarid,'FirstBin',int32(3));
    netcdf.putAtt(nc,avarid,'LastBin',int32(15));
    netcdf.putAtt(nc,avarid,'_FillValue', single(-32767.));
  case {'A2DC_IBR'}
%   cs = single([25 50:50:300 400 500 600 800 1000 1250 1500 2000 2500 ...
%     3000:1000:7000 ] );
    cvarid = netcdf.defVar(nc,[ 'C' v.name(2:end) ],xtype,dimids);
    for ivar = [ varid cvarid ]
%     netcdf.putAtt(nc,ivar,'CellSizes', cs);
      netcdf.putAtt(nc,ivar,'CellSizes', v.CellSizes);
      netcdf.putAtt(nc,ivar,'CellSizeUnits', 'micrometer');
      netcdf.putAtt(nc,ivar,'nDiodes',int32(32)) ;
      netcdf.putAtt(nc,ivar,'ResponseTime',0.4) ;
      netcdf.putAtt(nc,ivar,'ArmDistance',61.0) ;
      netcdf.putAtt(nc,ivar,'FirstBin',int32(1));
      netcdf.putAtt(nc,ivar,'LastBin',int32(20));
    end
    netcdf.putAtt(nc,cvarid,'long_name', '2DC concentration per bin');
    netcdf.putAtt(nc,cvarid,'units', '#/liter');
    netcdf.putAtt(nc,cvarid,'_FillValue', single(-32767));
  case {'A2DP_OBL'}
%   cs = single([100 200:200:2200 2600:400:3800 4400 5000 6000 8000 10000 ] );
    cvarid = netcdf.defVar(nc,[ 'C' v.name(2:end) ],xtype,dimids);
    for ivar = [ varid cvarid ]
%     netcdf.putAtt(nc,ivar,'CellSizes', cs);
      netcdf.putAtt(nc,ivar,'CellSizes', v.CellSizes);
      netcdf.putAtt(nc,ivar,'CellSizeUnits', 'micrometer');
      netcdf.putAtt(nc,ivar,'nDiodes',int32(32)) ;
      netcdf.putAtt(nc,ivar,'ResponseTime',0.4) ;
      netcdf.putAtt(nc,ivar,'ArmDistance',261.0) ;
      netcdf.putAtt(nc,ivar,'FirstBin',int32(1));
      netcdf.putAtt(nc,ivar,'LastBin',int32(20));
    end
    netcdf.putAtt(nc,cvarid,'long_name', '2DP concentration per bin');
    netcdf.putAtt(nc,cvarid,'units', '#/liter');
    netcdf.putAtt(nc,cvarid,'_FillValue',single(-32767));
  end
  try
    fill = netcdf.getAtt(nc,varid,'_FillValue');
  catch me
    netcdf.putAtt(nc,varid,'_FillValue',single(-32767));
  end
end
netcdf.endDef(nc);

%-----------------------------------------------------------
function writenc( nc, vars, gmeta, data )
g = netcdf.getConstant('NC_GLOBAL');

latnm = netcdf.getAtt(nc,g,'latitude_coordinate');
lonnm = netcdf.getAtt(nc,g,'longitude_coordinate');
altnm = netcdf.getAtt(nc,g,'zaxis_coordinate');

ymd = data(1,:);
hms = data(2,:);
times = datenum(1900+floor(ymd/10000),mod(floor(ymd/100),100),mod(ymd,100), ...
                floor(hms/10000),mod(floor(hms/100),100),mod(hms,100)) ...
      - gmeta.timeoff;
% Check for case where actual data exceed the times range in header
timediff=(times(1)-gmeta.starttm)*86400;
if timediff < 0
  fprintf('Deleting %d seconds of data at the start of the file.\n', ...
    round(-timediff));
  ii    = find(times>=gmeta.starttm);
  data  = data(:,ii);
  times = times(ii);
end
timediff=(times(end)-gmeta.endtm)*86400;
if timediff > 0
  fprintf('Deleting %d seconds of data at the end of the file.\n', ...
    round(timediff));
  ii    = find(times<=gmeta.endtm);
  data  = data(:,ii);
  times = times(ii);
end
idx = round((times-gmeta.starttm)*86400) + 1;

dimid   = netcdf.inqDimID(nc,'time' );
[~,nrec] = netcdf.inqDim(nc,dimid);
varid   = netcdf.inqVarID(nc,'time' );
netcdf.putVar(nc,varid,0:nrec-1);

for i = 1:length(vars)
  v = vars(i);
  if strcmp(v.name,'fsspsa')
    sa = data(v.col,1);
    disp(sprintf('FSSP sample area: %f',sa))
    avarid = netcdf.inqVarID(nc,'AFSSP_IBL');
    cvarid = netcdf.inqVarID(nc,'CFSSP_IBL');
    for id = [avarid,cvarid]
      netcdf.putAtt(nc,id,'SampleArea',round(sa*1000)/1000);
      diam = netcdf.getAtt(nc,id,'BeamDiameter');
      netcdf.putAtt(nc,id,'DepthOfField',round(sa/diam*1000)/1000);
    end
  else
    try
      varid = netcdf.inqVarID( nc, v.name );
      jj = v.col:v.col+v.RATE*v.vecsz-1;
      if v.vecsz > 1
        d = size(data,2);
        dt = zeros(v.vecsz+1,d);
        dt(2:end,:) = data(jj,:);
        if strcmp(v.name(1:6),'CFSSP_')
          fv = netcdf.getAtt(nc,varid,'_FillValue');
          ii = find(dt>0);
          dt(ii) = dt(ii) * 3;
          ii = find(dt(2,:)<0);
          dt(:,ii) = fv;
        end
        putData(nc,varid,idx,dt);
      else
        dt = data(jj,:);
        switch v.name
        case 'FRNG_IBL'
          if ~isempty(find(dt~=0))
            error('FSSP not in range 0.')
          end
        case {'FACT_IBL','FBMFR_IBL'}
          dt = dt / 100;
        case {'matime'}
          dt(dt<-888) = -888;
	case latnm
          netcdf.putAtt(nc,g,'geospatial_lat_min',single(min(dt(dt~=0))));
          netcdf.putAtt(nc,g,'geospatial_lat_max',single(max(dt(dt~=0))));
	case lonnm
          netcdf.putAtt(nc,g,'geospatial_lon_min',single(min(dt(dt~=0))));
          netcdf.putAtt(nc,g,'geospatial_lon_max',single(max(dt(dt~=0))));
	case altnm
          netcdf.putAtt(nc,g,'geospatial_vertical_min',single(min(dt(dt~=0))));
          netcdf.putAtt(nc,g,'geospatial_vertical_max',single(max(dt(dt~=0))));
        end
        putData(nc,varid,idx,dt);
      end
    catch me
      disp(['Not writing ' v.name]);
    end
  end
end

% Add A200X data
try
  cvar   = netcdf.inqVarID(nc,'C200X_OBR' );
  tasvar = netcdf.inqVarID(nc,'TASX' );
  avar   = netcdf.inqVarID(nc,'A200X_OBR' );
  conc   = squeeze(netcdf.getVar(nc,cvar));
  tas    = netcdf.getVar(nc,tasvar);
  ff1    = [.024 .225 .692 1.313 1.944  2.643 3.386 4.147 4.898 5.616 ...
             6.272 6.842 7.300 6.588 5.856];
  fv     = netcdf.getAtt(nc,cvarid,'_FillValue');
  igood  = find(conc(2,:) ~= fv);
  cnts   = fv * ones(size(conc));
  cnts(2:end,igood) = conc(2:end,igood) .* (ff1' * tas(igood)') / 1000;
  cnts(1,igood)     = 0;
  putData(nc,avar,idx,round(cnts));
catch ME
  whos
  disp('Unable to calculate A200X values');
  throw(ME)
end

% Add 2DC concentatrations
try
  avar   = netcdf.inqVarID(nc,'A2DC_IBR' );
  cs     = netcdf.getAtt(nc,avar,'CellSizes');
  cvar   = netcdf.inqVarID(nc,'C2DC_IBR' );
  cnts   = squeeze(netcdf.getVar(nc,avar));
  svol   = netcdf.getVar(nc,netcdf.inqVarID(nc,'svolc'));
  svol(svol<1) = 1;
  fv     = netcdf.getAtt(nc,avar,'_FillValue');
  igood  = find(cnts(2,:) ~= fv);
  conc   = fv * single(ones(size(cnts)));
  diam   = mean([cs(1:end-1);cs(2:end)]);
  afac   = ( 800.0 + diam ) / 800;
  conc(2:end,igood) = cnts(2:end,igood) ./ (afac'*svol(igood)');
  conc(1,igood) = 0;
  putData(nc,cvar,idx,conc);
catch ME
  whos
  disp('Unable to calculate 2DC values');
  throw(ME)
end

% Add 2DP concentatrations
try
  avar   = netcdf.inqVarID(nc,'A2DP_OBL' );
  cs     = netcdf.getAtt(nc,avar,'CellSizes');
  cvar   = netcdf.inqVarID(nc,'C2DP_OBL' );
  cnts   = squeeze(netcdf.getVar(nc,avar));
  svol   = netcdf.getVar(nc,netcdf.inqVarID(nc,'svolc'));
  svol(svol<1) = 1;
  fv     = netcdf.getAtt(nc,avar,'_FillValue');
  igood  = find(cnts(2,:) ~= fv);
  conc   = fv * single(ones(size(cnts)));
  diam   = mean([cs(1:end-1);cs(2:end)]);
  afac   = ( 6200.0 + diam ) / 6200;
  conc(2:end,igood) = cnts(2:end,igood) ./ (afac'*svol(igood)');
  conc(1,igood) = 0;
  putData(nc,cvar,idx,conc);
catch ME
  disp('Unable to calculate 2DP values');
  throw(ME)
end
  
  
%-----------------------------------------------------------
function newname = pmsvar( name )
twodp = 'OBL';
fssp  = 'IBL';
twodc = 'IBR';
onedc = 'OBR';
switch name
case 'fssp'
  newname = [ 'CONCF' '_' fssp ];
case 'fdbar'
  newname = [ 'DBARF' '_' fssp ];
case 'flwc'
  newname = [ 'PLWCF' '_' fssp ];
case 'vrej'
  newname = [ 'FBMFR' '_' fssp ];
case 'act'
  newname = [ 'FACT' '_' fssp ];
case 'range'
  newname = [ 'FRNG' '_' fssp ];
case 'strobes'
  newname = [ 'FSTB' '_' fssp ];
case 'nfssp'
  newname = [ 'AFSSP' '_' fssp ];
case 'fsspc'
  newname = [ 'CFSSP' '_' fssp ];
case 'oned'
  newname = [ 'C200X' '_' onedc ];
case 'onedc'
  newname = [ 'CONCX' '_' onedc ];
case 'nszic'
  newname = [ 'A2DC' '_' twodc ];
case 'nszip'
  newname = [ 'A2DP' '_' twodp ];
otherwise
  newname = name;
end
  
%-----------------------------------------------------------
function putData( nc, varid, idx, data )
[varname,xtype,dimids] = netcdf.inqVar(nc,varid);
sz=[];
for i=1:length(dimids)
  [dimsize,dimlen] = netcdf.inqDim(nc,dimids(i));
  sz = [dimlen sz];
end
if length(dimids) == 1; sz=[sz 1]; end
fv = netcdf.getAtt(nc,varid,'_FillValue');
dt = fv * ones(sz);
dt = squeeze(dt);
if length(dimids) == 3
  data = squeeze(data)';
% varname
% whos
  if size(data) == size(dt); idx  = 1:size(data,1); end
% mxd = max(data)
% mnd = min(data)
end
dt(idx,:) = data;
netcdf.putVar(nc,varid,dt');

