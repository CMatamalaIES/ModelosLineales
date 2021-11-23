clc
clear all
% 
cd 'C:\Users\Cobal\Documents\Sexto Semestre 2021\Modelos Lineales Matlab'
datos = xlsread('base2.xlsx');


X_1 = datos(:,2);
X_2 = datos(:,3);
X_3 = datos(:,4);
lX_2 = log(X_2);
y = datos(:,1);
X1 = [X_1 X_2 X_3];
X2 = [X_1 lX_2 X_3];


b_bar = [0 0 0 0]';
R = [1 0 0 0;
     0 1 0 0;
     0 0 1 0;
     0 0 0 1];
r = [0 0 0 0]';

Beta_L1 = Beta(X1,y,1,b_bar,R,r);
Beta_l1 = Beta(X2,y,1,b_bar,R,r);

figure(1)
subplot(3,1,1)
scatter(X_1,y)
title('Distribucion Acceso Agua vs Esperanza de vida')
subplot(3,1,2)
scatter(X_2,y)
title('Distribucion Numero de doctores vs Esperanza de vida')
subplot(3,1,3)
scatter(X_3,y)
title('Distribucion Gasto en Salud vs Esperanza de vida')

