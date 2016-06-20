#include <stdio.h>
#include "util.h"
#include "RocketToFFTTester.h"

// gcc -o Untitled Untitled.c

int main( int argc, char* argv[] ){

    //////////// TEST MEMORY -- REMOVE
    //////////// END TEST MEMORY

    int testNum, frameNum, currN, currNumFrames, idxStart, idxEnd, idxCount, success;
    unsigned long io, currIsFFT, currFFTIdx;

    for (testNum = 0; testNum < NUM_TESTS; testNum++){

        currN = fftn[testNum];
        currIsFFT = isFFT[testNum];
        currFFTIdx = fftIdx[testNum];
        currNumFrames = frames[testNum];
        printf(
            "FFT N = %d \t Frames To Be Tested = %d \t FFT? = %lu \t Idx = %lu \n",
            currN,currNumFrames,currIsFFT,currFFTIdx
        );
        success = setup(currFFTIdx,currIsFFT);
        if (success == 0) return -1;

        for(frameNum = 0; frameNum < currNumFrames; frameNum++){

            idxStart = frameNum * currN;
            idxEnd = idxStart + currN - 1;
            printf(
                "FFT N = %d \t Frame = %d \t Test vectors -- \t Starting Idx: %d \t Ending Idx: %d \n",
                currN,frameNum,idxStart,idxEnd
            );

            success = load(testNum,idxStart,currN);
            if (success == 0) return -2;

            calculate();








/*
            for(idxCount = 0; idxCount < currN; idxCount++){
                ioLoc = outptr[testNum] + idxStart + idxCount;
                printf("Output: %lu \n", *(ioLoc));
            }*/





        }
    }
    return 0;
}

int setup(unsigned long currFFTIdx, unsigned long currIsFFT)
{
    unsigned long o;
    test[fftIdx_BASE] = currFFTIdx;
    test[isFFT_BASE] = currIsFFT;
    test[setupDone_BASE] = START;
    o = test[setupDone_BASE];
    while(o != DONE) {
        o = test[setupDone_BASE];
    }
    if (test[fftIdx_BASE] != currFFTIdx){
        printf(">> Setup Idx incorrect! \n");
        return 0;
    }
    if (test[isFFT_BASE] != currIsFFT){
        printf(">> Setup FFT/IFFT type incorrect! \n");
        return 0;
    }
    printf("Setup done! \n");
    return 1;
}

int load(int testNum, int idxStart, int currN)
{
    int idxCount;
    unsigned long *loc, inVal;
    for(idxCount = 0; idxCount < currN; idxCount++){
        loc = inptr[testNum] + idxStart + idxCount;
        inVal = *(loc);
        // REMOVE
        printf("Input: %lu \n", inVal);
        test[toFFT_BASE + idxCount] = inVal;
    }
    for(idxCount = 0; idxCount < currN; idxCount++){
        loc = inptr[testNum] + idxStart + idxCount;
        inVal = *(loc);
        if (test[toFFT_BASE + idxCount] != inVal){
            printf(
                ">> Loaded value incorrect for FFTN = %d \t Input # = %d",
                currN,(idxStart + idxCount)
            );
            return 0;
        }
    }
    printf("Loading done! \n");
    return 1;
}

void calculate (void) {
    unsigned long o;
    test[calcDone_BASE] = START;
    o = test[calcDone_BASE];
    while(o != DONE) {
        o = test[calcDone_BASE];
    }
    printf("Done calculating! \n");
}







// test AAA
// test k, isFFT, etc.











// compare (keep track of frame)
// base address
// need scala tester out -- check for neg
// send 16xA