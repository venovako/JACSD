function Z = d2z(D)
dsz=size(D);
ssz=size(dsz);
ndz=ssz(1,2);
switch ndz
    case 2
        Z=complex(squeeze(D(1,:)), squeeze(D(2,:)));
    case 3
        Z=complex(squeeze(D(1,:,:)), squeeze(D(2,:,:)));
    case 4
        Z=complex(squeeze(D(1,:,:,:)), squeeze(D(2,:,:,:)));
    case 5
        Z=complex(squeeze(D(1,:,:,:,:)), squeeze(D(2,:,:,:,:)));
    case 6
        Z=complex(squeeze(D(1,:,:,:,:,:)), squeeze(D(2,:,:,:,:,:)));
    case 7
        Z=complex(squeeze(D(1,:,:,:,:,:,:)), squeeze(D(2,:,:,:,:,:,:)));
    otherwise
        error('d2z: dimension %d not implemented.',ndz);
end
end