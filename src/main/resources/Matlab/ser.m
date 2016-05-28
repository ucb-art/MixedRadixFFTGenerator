%% BER/SER (http://www.mathworks.com/help/comm/gs/compute-ber-for-a-qam-system-with-awgn-using-matlab.html
%  For OFDM specific, see http://www.engr.usask.ca/classes/EE/456/assignments/OFDM-Simulation-EE810.pdf

format long g

close all
clear all
clc

eqThreshold = 5e-6

% M-QAM
M = 16
% Table should match Demod table! -- Maps least to greatest 
% TODO: Matlab should control Chisel SER Analysis
qam16Table = [0 2 3 1]
% FFT N (# of symbols per frame)
N = 128
% # of frames
frames = 333
% The ratio of bit energy to noise power spectral density, in dB (Eb/No)
EbNo = 0:2:12;

% Theoretical BER, SER
[ber, ser] = berawgn(EbNo,'qam',M);

k = log2(M);                                    % # bits per symbol
numBits = k*N*frames;
numSamplesPerSymbol = 1;                        % Oversampling factor

rng default                                     % Use default random # generator
din = randi([0 1],numBits,1);                   % Vec of binary data
dinSymbols = bi2de(reshape(din,frames*N,k));    % Convert to data symbols (reshape: # rows = N * frames)

% (Matlab) range of bits for I,Q
quadrange = 1:1:length(qam16Table);
% Starts @ top,left, then goes down columns and from left to right
qUD = flipud(quadrange');
iLR = quadrange;
qamMap = zeros(M,1);
for i=1:1:length(iLR)
    for j=1:1:length(qUD)
       % Q corresponds to MSBs, I coresponds to LSBs
       idx = (i-1)*length(qUD) + (j-1) + 1;
       qamMap(idx) = qam16Table(qUD(j))*length(iLR) + qam16Table(iLR(i));
    end
end

infull = 0:1:M-1;
set(gca,'fontsize',18)
qammod(infull,M,qamMap,'PlotConstellation',true).'
set(gca,'fontsize',18)

fig=gcf;
set(findall(fig,'-property','FontSize'),'FontSize',14)

% txout = qammod(dinSymbols,M,0,'gray');          % Gray coding, phase offset = 0 QAM mod
qammodout = qammod(dinSymbols,M,qamMap);

snr = EbNo + 10*log10(k) - 10*log10(numSamplesPerSymbol);

% Pre-allocate outputs
rxin = zeros(frames*N,length(snr));
rxintime = zeros(frames*N,length(snr));
doutSymbols = zeros(frames*N,length(snr));
dout = zeros(numBits,length(snr));
real_ser = zeros(1,length(snr));
real_ber = zeros(1,length(snr));

% time-domain tx out (see OFDM link for Matlab IFFT -> standard to satisfy parseval's)
origfreqpow = sum(abs(qammodout).^2);
txout = sqrt(N)*reshape(ifft(reshape(qammodout, N, frames)),N*frames,1);
timepow = sum(abs(txout).^2);
noiselessfreq = (1/sqrt(N))*reshape(fft(reshape(txout, N, frames)),N*frames,1);
noiselessfreqpow = sum(abs(noiselessfreq).^2);

if (~(abs(noiselessfreqpow-origfreqpow)<eqThreshold && abs(noiselessfreqpow-timepow)<eqThreshold))
    display('Freq <-> Time domain waveforms have different powers?!?!')
end

% Adds different amounts of noise to modulated signal
% Demodulates the signal
% Calculates SER & BER (after converting from decimal to binary)
for i=1:length(snr)
    rxintime(:,i) = awgn(txout,snr(i),'measured');
    rxin(:,i) = (1/sqrt(N))*reshape(fft(reshape(rxintime(:,i), N, frames)),N*frames,1);
    % doutSymbols(:,i) = qamdemod(rxin(:,i),M,0,'gray');
    doutSymbols(:,i) = qamdemod(rxin(:,i),M,qamMap);
    % second term is ratio of errors to # of symbols
    [numSymErr,real_ser(i)] = symerr(dinSymbols,doutSymbols(:,i)); 
    % output data as column vectors (for each snr)
    temp = de2bi(doutSymbols(:,i),k);
    dout(:,i) = temp(:);
    [numBitErr,real_ber(i)] = biterr(din,dout(:,i));
end

% Plot constellation with highest SNR (noise corrupted + transmitted)
constellation = scatterplot(rxin(:,length(snr)),1,0,'g.');
hold on
scatterplot(qammodout,1,0,'k*',constellation)

% BER plot
figure; semilogy(snr,real_ber,'k*-');
hold on; semilogy(snr,ber,'ro-');
title('Simulated BER Compared with Theoretical BER');
legend('Simulated BER',...
   'Theoretical BER','Location','SouthWest');
xlabel('SNR (dB)');
ylabel('BER');

% SER plot
%figure; semilogy(snr,real_ser,'*-');
%hold on; semilogy(snr,ser,'o-');
%title('Simulated SER Compared with Theoretical SER');
%legendArraySER = {'Simulated SER','Theoretical SER'};
%legend(legendArraySER,'Location','SouthWest');
%xlabel('SNR (dB)');
%ylabel('SER');

% SER plot
figure; semilogy(snr,real_ser,'*-','LineWidth',2,'MarkerSize',8);
hold on; semilogy(snr,ser,'o-','LineWidth',2,'MarkerSize',8);
title('Simulated SER Compared with Theoretical SER');
legendArraySER = {'Simulated SER','Theoretical SER'};
legend(legendArraySER,'Location','SouthWest');
xlabel('SNR (dB)');
ylabel('SER');
set(gca,'FontSize',16);
grid on;

% FFT input vectors
testin = reshape(rxintime,frames*N*length(snr),1);

% testout = reshape(doutSymbols,frames*N*length(snr),1);

fid = fopen('generated/in_i.txt','wt');       % writing in text mode
fprintf(fid,'%f\n',real(testin));   
fclose(fid);
fid = fopen('generated/in_q.txt','wt');       % writing in text mode
fprintf(fid,'%f\n',imag(testin));   
fclose(fid);

% fid = fopen('generated/demod_out.txt','wt');  % writing in text mode
% fprintf(fid,'%d\n',testout);   
% fclose(fid);

% fid = fopen('generated/in_symbols.txt','wt'); % writing in text mode
% fprintf(fid,'%d\n',dinSymbols);   
% fclose(fid);

% params = [M N frames]
% fid = fopen('generated/demod_params.txt','wt');  % writing in text mode
% fprintf(fid,'%d\n',params);   
% fclose(fid);

%% Process Chisel results

markers = {'+','x','s','d','^','v','>','<','p','h','*','o'};
chiseldump = dir('../../../../build/analysis/ser*');
chiselser = zeros(length(chiseldump),length(snr));
% # of Chisel outputs corresponds to # of fixed pt designs + 1 dbl tested
for i=1:1:length(chiseldump)
    fileName = chiseldump(i).name;
    newLabel = regexp(fileName,'(?<=_).*(?=.txt)','match');
    legendName = newLabel;
    if strcmp(legendName(1),'Dbl')
        legendName = 'Chisel Double';
    else
        legendName = strcat(legendName, ' Bit IO');
    end
    legendArraySER = [legendArraySER, legendName];
   
    %legendArraySER = [legendArraySER, newLabel];
    fileLoc = ['../../../../build/analysis/' fileName];
    chiselfft = fscanf(fopen(fileLoc,'r'),'%f');
    % Group SER checks by SNR used
    chiselfftgroupedbysnr = reshape(chiselfft,length(chiselfft)/length(snr),length(snr));
    
    % Check Chisel Dbl result matches Matlab simulation within error threshold
    if (strcmp(newLabel,'Dbl'))
        chiselfftreal = chiselfft(1:2:end);
        chiselfftimag = chiselfft(2:2:end);
        chiselfftout = complex(chiselfftreal,chiselfftimag);
        matlabfftout = reshape(rxin,frames*N*length(snr),1);
        if (sum(abs(chiselfftout - matlabfftout) > eqThreshold) > 0)
            display('Matlab and Chisel Dbl FFT do not match! :(')
        else
            display('Matlab and Chisel Dbl FFT match! :)')
        end
    end
    
    for j=1:length(snr)
        chiselfftout = chiselfftgroupedbysnr(:,j);
        % Chisel dumps as r0,i0,r1,i1,...
        chiselfftreal = chiselfftout(1:2:end);
        chiselfftimag = chiselfftout(2:2:end);
        chiselfftout = complex(chiselfftreal,chiselfftimag);
        demodout = qamdemod(chiselfftout,M,qamMap);
        % second term is ratio of errors to # of symbols
        [numSymErr,chiselser(i,j)] = symerr(dinSymbols,demodout); 
    end 
    
    semilogy(snr,chiselser(i,:),[markers{i} '-'],'LineWidth',2,'MarkerSize',8);
    legend(legendArraySER,'Location','best');
    
end