function D = z2d(Z)
zsz=size(Z);
dsz=[2 zsz];
D=zeros(dsz);
ssz=size(zsz);
ndz=ssz(1,2);
switch ndz
    case 1
        D(1,:)=real(Z);
        D(2,:)=imag(Z);
    case 2
        D(1,:,:)=real(Z);
        D(2,:,:)=imag(Z);
    case 3
        D(1,:,:,:)=real(Z);
        D(2,:,:,:)=imag(Z);
    case 4
        D(1,:,:,:,:)=real(Z);
        D(2,:,:,:,:)=imag(Z);
    case 5
        D(1,:,:,:,:,:)=real(Z);
        D(2,:,:,:,:,:)=imag(Z);
    case 6
        D(1,:,:,:,:,:,:)=real(Z);
        D(2,:,:,:,:,:,:)=imag(Z);
    otherwise
        error('z2d: dimension %d not implemented.',ndz);
end
end