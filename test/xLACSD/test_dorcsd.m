function test_dorcsd(fn)
f=fopen(fn,'r');
ist=fread(f,[4],'int32');
m=ist(1);
p=ist(2);
q=ist(3);
e=ist(4);
r=min([p m-p q m-q]);
disp('[M P Q R]');
disp([m p q r]);
theta=fread(f,[r],'double');
disp('THETA');
disp(theta);
c=fread(f,[r],'double');
if e == 0
    disp('COS');
else
    disp('PHI');
end
disp(c);
if e == 0
    s=fread(f,[r],'double');
    disp('SIN');
    disp(s);
    a=abs((c.*c+s.*s)-1);
    disp('max|c^2+s^2 - 1|');
    disp(max(a));
    disp('||c^2+s^2 - 1||');
    disp(norm(a));
    Y=fread(f,[m m],'double');
    U=fread(f,[m m],'double');
    VT=fread(f,[m m],'double');
    X=U*Y*VT;
    I=eye(m);
    A=X'*X-I;
    disp('max|X^T X - I|');
    disp(max(max(abs(A))));
    disp('||X^T X - I||_F');
    disp(norm(A,'fro'));
    A=X*X'-I;
    disp('max|X X^T - I|');
    disp(max(max(abs(A))));
    disp('||X X^T - I||_F');
    disp(norm(A,'fro'));
end
fclose(f);
end