clear;

fnheader = 'Star_WENO_';
fnvar = 'Pressure';
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
        for ix = 1:1000
            strtmp = fgetl(fid);
            if(length(strtmp)==0)
                break;
            end
            x(ix) = str2num(strtmp(1:33));
            var(ix) = str2num(strtmp(34:66));
        end
    end

end

    figure(1)
    plot(x,var)
    axis([0 1 -0.5 1.3])


