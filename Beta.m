function [Beta,R2,t,F] = Beta(X,y,c,b_bar,R,r)
%--------------------------------------------------------------------------
%Proposito: Encontrar los coeficientes que acompañan a las variables alea-
%           torias. Tambien la función entrega el R2 que es el poder de es-
%           timación del modelo propuesto, 
%--------------------------------------------------------------------------
%Input: X: Matriz nxK que representa a las variables independientes o expli
%          catorias.
%       y: Vector nx1 que representa a las variables dependientes.
%       c: Matriz 1x1 que toma dos valores:
%          c = 1; cuando el modelo tiene una constante.
%          c = 0; cuando el modelo no tiene constante.
%       b_bar: Vector Kx1 que contiene las hipotesis nulas para el testeo.
%       R: Matriz rxK que tiene las ecuaciones para la prueba de hipotesis.
%       r: Vector rx1 que tiene los valores de las hipotesis nulas.
%--------------------------------------------------------------------------
%Output: Beta: Vector nx1 que representa el valor de los coeficientes que 
%              acompañan a la variable independiente.
%        R2  : Matriz 1x1 que representa el valor R cuadrado, que indica 
%              que tan bueno es nuestro modelo.
%        t   : Vector nx1 que muestra los minimos valores para rechazar la
%              hipotesis nula.
%        F   : Matriz 1x1 test F
%--------------------------------------------------------------------------
if nargin <3
    c = 1;
else
    if nargin <4
        b_bar = [0 0 0]';
    end
end
n = length(y);
K = size(X,2);
t = NaN(K,1);
switch c
    
    case 0
        % Encontrando los betas
        Beta = (X'*X)^(-1)*X'*y;
        % Calculando R2
        y_hat = X*Beta;
        R2 = sum(y_hat.^2)/sum(y.^2);
        % Test de hipotesis
        e = y-y_hat;
        e2 = sum(e.^2);
        s2 = e2/(n-K);
        V = s2*inv(X'*X);
        SE = sqrt(V);
        for i=1:K;
           t(i,1) = (Beta(i,1)-b_bar(i,1))/SE(i,i);
        end
        numeral_r = size(r,1);
        F = [(R*Beta-r)'*inv(R*V*R')*(R*Beta-r)]/numeral_r;
        VarNames = {'beta', 't'};
        T = table(Beta(:,1),t(:,1), 'VariableNames',VarNames)

        VarNames2 = {'R2', 'F', 'n', };
        T2 = table(R2,F,n, 'VariableNames',VarNames2)
               
    case 1
        % Agregamos la columna que representa la constante
        X = [ones(length(X),1) X];
        % Calculando R2
        K = size(X,2);
        t = NaN(K,1);
        Beta = (X'*X)^(-1)*X'*y;
        y_hat = X*Beta;
        R2 = sum((y_hat-mean(y)).^2)/sum((y-mean(y)).^2);
        % Test de hipotesis
        e = y-y_hat;
        e2 = sum(e.^2);
        s2 = e2/(n-K);
        V = s2*inv(X'*X);
        SE = sqrt(V);
        for i=1:K;
           t(i,1) = (Beta(i,1)-b_bar(i,1))/SE(i,i);
        end
        numeral_r = size(r,1);
        F = [(R*Beta-r)'*inv(R*V*R')*(R*Beta-r)]/numeral_r;
        
        VarNames = {'beta', 't'};
        T = table(Beta(:,1),t(:,1), 'VariableNames',VarNames)

        VarNames2 = {'R2', 'F', 'n', };
        T2 = table(R2,F,n, 'VariableNames',VarNames2)
        
end