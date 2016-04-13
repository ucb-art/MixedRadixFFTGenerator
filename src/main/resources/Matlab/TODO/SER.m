%% setup javapaths from Arbor
close all
clear all
clear java
clear classes
clear functions
pack

%%
javapaths

% correctness threshold
threshold = 0.000000001;

% N = FFT frame size, M = Modulation Order
N=4; 
M=4; 

%% check FFT

inreal = [1 1 1 1];
inimag = [1 1 1 1];

chiselfft = FFT.runMatlabDouble(N,inreal,inimag);
chiselfftreal = chiselfft(1:2:end);
chiselfftimag = chiselfft(2:2:end);
chiselfft = complex(chiselfftreal,chiselfftimag);

matlabfft = fft(complex(inreal,inimag)).'

%%

% Create input data (from symbols + noise corruption)
EsN0dBs = linspace(10, 30, 10);
EsN0s = 10.^(EsN0dBs./10);
k = 1 / sqrt (2/3.0 * (M - 1));
Es = 2 / 3 * (M - 1);
% numTrials = number of symbols (not frames)
numtrials = N * 100;
maxSymbol = sqrt(M)-1
raw_symbols = (randi([0 maxSymbol], 1, numtrials) * 2 - maxSymbol) + j*(randi([0 maxSymbol], 1, numtrials) * 2 - maxSymbol);
symbols = reshape(ifft(reshape(raw_symbols, N, numtrials/N)), 1, numtrials);

to_send   = [];

for EsN0=EsN0s
    N0 = Es / EsN0 / N;
    noise  = randn(1, length(symbols)) + ...
             i * randn(1, length(symbols));
    to_send = [to_send (k * sqrt(Es) * symbols + sqrt(N0 / 2) * noise)];
end

% Perform ideal + nonideal FFTs on input data
matlabfft  = reshape(fft(reshape(to_send, N, length(to_send)/N)), 1, length(to_send)).';

% Chisel FFT (double precision) 
chiselfft = FFT.runMatlabDouble(N,real(to_send), imag(to_send));
chiselfftreal = chiselfft(1:2:end);
chiselfftimag = chiselfft(2:2:end);
chiselfft = complex(chiselfftreal,chiselfftimag);

diff = abs(matlabfft-chiselfft);
isequal = max(diff) < threshold

%% Demod

% Ideal demod (no noise)
res = qamdemod(raw_symbols,M);
res = repmat(res, 1, length(EsN0s)).';

% Output of demod following FFTs
matlabrecv = qamdemod(matlabfft,M);
chiselrecv = qamdemod(chiselfft,M);

ideal = 2*(1 - 1/sqrt(M)) * erfc(k .* sqrt(EsN0s)) - ...
    (1 - 2/sqrt(M)+1/M) .* (erfc(k .* sqrt(EsN0s))).^2;

notequal = res ~= chiselrecv;
matlabneq = res ~= matlabrecv;
errors   = sum(reshape(notequal, numtrials, length(EsN0s)), 1);
matlaberrors = sum(reshape(matlabneq, numtrials, length(EsN0s)), 1);
error_rate = errors / numtrials;
matlab_error_rate = matlaberrors / numtrials;
semilogy(EsN0dBs, error_rate, EsN0dBs, matlab_error_rate, EsN0dBs, ideal);
title('FFT+Demod Symbol Error Rate')
xlabel('Es/N0 (dB)')
ylabel('Symbol Error Rate')
legend('Sim', 'Matlab FFT', 'Ideal')