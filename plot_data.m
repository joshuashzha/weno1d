clear;close all;

fnheader = 'Star_WENO_';
fnvar = 'Density';
fnposix = '_1.dat';

fn = [fnheader,fnvar,fnposix];

fid = fopen(fn);

stp = 0;
while(fid)
    strtmp=fgetl(fid);
    if(strtmp<0) 
        break;
    elseif(strtmp(2) == '"')
        stp = stp+1;
        time(stp) = str2num(strtmp(10:35));
        for ix = 1:10000
            strtmp = fgetl(fid);
            if(length(strtmp)==0)
                break;
            end
            x(ix) = str2num(strtmp(1:38));
            var(ix) = str2num(strtmp(39:76));
        end
    end
    figure(1)
    plot(x,var)
    %axis([0 1 -0.5 1.3])
    hold on
end



