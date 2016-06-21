#include <stdio.h>
#include "util.h"
#include "RocketToFFTTester.h"

// gcc -o Untitled Untitled.c

// Volatile pointer to a volatile variable
volatile long *volatile test = (long *) TEST_BASE;

int main( int argc, char* argv[] ){

    int testNum, frameNum;
    int currN, currNumFrames;
    int idxStart, idxEnd;
    int success;
    unsigned long currIsFFT, currFFTIdx, k;
    volatile unsigned long o;

    for (testNum = 0; testNum < NUM_TESTS; testNum++){

        currN = fftn[testNum];
        currNumFrames = frames[testNum];

        currIsFFT = isFFT[testNum];
        currFFTIdx = fftIdx[testNum];

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

            // REMOVE
            // test[k_OFFSET] = currN-1;

            o = test[k_OFFSET];
            k = currN-1;
            if (o != k) {
                printf("k incorrect, \t Expected = %ld \t Out = %ld \n ",k,o);
                return -3;
            }
            else printf("k is correct! \n");

            // REMOVE
            // success = loadOutput(testNum,idxStart,currN);
            // if (success == 0) return -5;

            success = checkOut(testNum,idxStart,currN);
            if (success == 0) return -4;

        }
    }
    printf("Testbench passed! :) \n");
    return 0;
}

int setup(unsigned long currFFTIdx, unsigned long currIsFFT)
{
    volatile unsigned long o;
    test[fftIdx_OFFSET] = currFFTIdx;
    test[isFFT_OFFSET] = currIsFFT;
    test[setupDone_OFFSET] = START;
    o = test[setupDone_OFFSET];
    while(o != DONE) {
        printf("Setup not done, \t o = %ld \n",o);
        o = test[setupDone_OFFSET];
    }
    printf("Setup done! \t o = %ld \n",o);
    o = test[fftIdx_OFFSET];
    if (o != currFFTIdx){
        printf(">> Setup fftIdx incorrect! \t Expected = %ld \t Out = %ld \n", currFFTIdx, o);
        return 0;
    }
    else printf("fftIdx correct! \n");
    o = test[isFFT_OFFSET];
    if (o != currIsFFT){
        printf(">> Setup FFT/IFFT type incorrect! \t Expected = %ld \t Out = %ld \n", currIsFFT, o);
        return 0;
    }
    else printf("isFFT correct! \n");

    return 1;
}

int load(int testNum, int idxStart, int currN)
{
    volatile unsigned long o;
    int idxCount;
    unsigned long *loc, inVal;
    for(idxCount = 0; idxCount < currN; idxCount++){
        loc = inptr[testNum] + idxStart + idxCount;
        inVal = *loc;
        // REMOVE
        // printf("Input: %ld \n", inVal);
        test[toFFT_OFFSET + idxCount] = inVal;
    }
    for(idxCount = 0; idxCount < currN; idxCount++){
        loc = inptr[testNum] + idxStart + idxCount;
        inVal = *loc;
        o = test[toFFT_OFFSET + idxCount];
        if (o != inVal){
            printf(
                ">> Loaded value incorrect for FFTN = %d \t Input # = %d \t Expected = %ld \t Out = %ld \n",
                currN,(idxStart + idxCount),inVal,o
            );
            return 0;
        }
        // REMOVE
        // else printf("Input -> Output : %ld \n", o);
    }
    printf("Loading (inputs) done! \n");
    return 1;
}

void calculate (void) {
    volatile unsigned long o;
    test[calcDone_OFFSET] = START;
    o = test[calcDone_OFFSET];
    while(o != DONE) {
        printf("Calculation not done, \t o = %ld \n",o);
        o = test[calcDone_OFFSET];
    }
    printf("Calculation done! \t o = %ld \n",o);
}

// REMOVE
/*
int loadOutput(int testNum, int idxStart, int currN)
{
    int idxCount;
    unsigned long *loc, outVal;
    for(idxCount = 0; idxCount < currN; idxCount++){
        loc = outptr[testNum] + idxStart + idxCount;
        outVal = *loc;
        printf("Load output: %ld \n", outVal);
        test[fromFFT_OFFSET + idxCount] = outVal;
    }
    printf("Loading (outputs) done! \n");
    return 1;
}
*/

int checkOut(int testNum, int idxStart, int currN)
{
    volatile unsigned long o;
    int idxCount;
    unsigned long *loc, outVal;
    for(idxCount = 0; idxCount < currN; idxCount++){
        loc = outptr[testNum] + idxStart + idxCount;
        outVal = *loc;
        o = test[fromFFT_OFFSET + idxCount];
        if (o != outVal){
            printf(
                ">> Output value incorrect for FFTN = %d \t Output # = %d \t Expected = %ld \t Out = %ld \n",
                currN,(idxStart + idxCount),outVal,o
            );
            return 0;
        }
        // REMOVE
        // else printf("Output : %ld \n", o);
    }
    printf("Output check complete! \n");
    return 1;
}