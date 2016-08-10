function test_zuncsd(fn)
f=fopen(fn,'r');
ist=fread(f,[4],'int32');
m=ist(1);
p=ist(2);
q=ist(3);
e=ist(4);
r=min([p m-p q m-q]);
disp('[M P Q R]');
disp([m p q r]);
tr=fread(f,[2 r],'double');
theta=d2z(tr);
disp('THETA');
disp(theta);
cr=fread(f,[2 r],'double');
c=d2z(cr);
if e == 0
    disp('COS');
else
    disp('PHI');
end
disp(c);
if e == 0
    sr=fread(f,[2 r],'double');
    s=d2z(sr);
    disp('SIN');
    disp(s);
    a=abs((c.*c+s.*s)-1);
    disp('max|c^2+s^2 - 1|');
    disp(max(a));
    disp('||c^2+s^2 - 1||');
    disp(norm(a));
    Yr=reshape(fread(f,[2*m m],'double'),[2 m m]);
    Y=d2z(Yr);
    Ur=reshape(fread(f,[2*m m],'double'),[2 m m]);
    U=d2z(Ur);
    VTr=reshape(fread(f,[2*m m],'double'),[2 m m]);
    VT=d2z(VTr);
    X=U*Y*VT;
    I=eye(m)+1i*zeros(m);
    A=X'*X-I;
    disp('max|X^H X - I|');
    disp(max(max(abs(A))));
    disp('||X^H X - I||_F');
    disp(norm(A,'fro'));
    A=X*X'-I;
    disp('max|X X^H - I|');
    disp(max(max(abs(A))));
    disp('||X X^H - I||_F');
    disp(norm(A,'fro'));
end
fclose(f);
end