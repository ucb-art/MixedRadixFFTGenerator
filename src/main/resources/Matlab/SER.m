EsN0dBs      = linspace(10, 30, 10);
EsN0s     = 10.^(EsN0dBs./10);
N=4; %% FFT frame size
M=4; %% Modulation Order
k = 1 / sqrt (2/3.0 * (M - 1));
Es = 2 / 3 * (M - 1);
numtrials = N * 100;

raw_symbols = (randi([0 15], 1, numtrials) * 2 - 15) + j*(randi([0 15], 1, numtrials) * 2 -15);
symbols = reshape(ifft(reshape(raw_symbols, N, numtrials/N)), 1, numtrials);

to_send   = [];

for EsN0=EsN0s
    N0 = Es / EsN0 / N;
    noise  = randn(1, length(symbols)) + ...
             i * randn(1, length(symbols));
    to_send = [to_send (k * sqrt(Es) * symbols + sqrt(N0 / 2) * noise)];
end



matlabfft  = reshape(fft(reshape(to_send, N, length(to_send)/N)), 1, length(to_send));


angiefft = FFT.runMatlabDouble(N,real(to_send), imag(to_send));

%res = basebandDemod.run(M, real(raw_symbols), imag(raw_symbols));
%res = repmat(res', 1, length(EsN0s));
%%
matlabrecv = basebandDemod.run(M, real(matlabfft), imag(matlabfft))';
ideal = 2*(1 - 1/sqrt(M)) * erfc(k .* sqrt(EsN0s)) - ...
    (1 - 2/sqrt(M)+1/M) .* (erfc(k .* sqrt(EsN0s))).^2;

notequal = res ~= recv;
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