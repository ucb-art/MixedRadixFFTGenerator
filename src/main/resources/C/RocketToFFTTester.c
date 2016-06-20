#include <stdio.h>
#include "util.h"
#include "RocketToFFTTester.h"

// gcc -o Untitled Untitled.c

int main( int argc, char* argv[] ){

    //////////// TEST MEMORY
    test[setupDone_ADDR] = DONE;
    printf("Done?: %lu \n",test[setupDone_ADDR]);
    //////////// END TEST MEMORY

    int testNum, frameNum, currN, currNumFrames, idxStart, idxEnd, idxCount, success;
    unsigned long io, currIsFFT, currFFTIdx, *ioLoc;

    for (testNum = 0; testNum < NUM_TESTS; testNum++){
        currN = n[testNum];
        currIsFFT = isFFT[testNum];
        currFFTIdx = fftIdx[testNum];
        currNumFrames = frames[testNum];
        printf(
            "N = %d \t Frames Tested = %d \t FFT? = %lu \t Idx = %lu \n",
            currN,currNumFrames,currIsFFT,currFFTIdx
        );
        success = setup(currFFTIdx,currIsFFT);
        if (success == 0) return -1;









        for(frameNum = 0; frameNum < currNumFrames; frameNum++){
            idxStart = frameNum * currN;
            idxEnd = idxStart + currN - 1;
            printf(
                "Test vectors: \t Starting Idx: %d \t Ending Idx: %d \n",
                idxStart,idxEnd
            );
            for(idxCount = 0; idxCount < currN; idxCount++){
                ioLoc = inptr[testNum] + idxStart + idxCount;
                printf("Input: %lu \n", *(ioLoc));
            }
            for(idxCount = 0; idxCount < currN; idxCount++){
                ioLoc = outptr[testNum] + idxStart + idxCount;
                printf("Output: %lu \n", *(ioLoc));
            }
        }
    }
    return 0;
}

int setup(unsigned long currFFTIdx, unsigned long currIsFFT)
{
    unsigned long o;
    test[fftIdx_ADDR] = currFFTIdx;
    test[isFFT_ADDR] = currIsFFT;
    test[setupDone_ADDR] = START;
    o = test[setupDone_ADDR];
    while(o != DONE) {
        o = test[setupDone_ADDR];
        printf("Done?: %lu \n",o);
    }
    if (test[fftIdx_ADDR] != currFFTIdx){
        printf(">> Setup Idx incorrect!");
        return 0;
    }
    if (test[isFFT_ADDR] != currIsFFT){
        printf(">> Setup FFT/IFFT type incorrect!");
        return 0;
    }
    return 1;
}













// setup function --> wait for setup done (addr)
// load (need to keep track of frame mod)
// calculate
// compare (keep track of frame)
// base address
// need scala tester out