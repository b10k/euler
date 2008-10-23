/* p213.c
 * $Id:$
 */

#include <stdio.h>
#include <stdlib.h>

#define side    30

int main(int argc, char **argv) {
    int i;
    double ***board = malloc(side*side*side*side*2*sizeof(double))

    return(0);
}

updateOneBoard(double *src, double *dst)
{
    int i,j;
    double *pUp, *pDown;
    double *pSrc = src;
    double *pDst = dst;

    /* First handle the top row separately */
    pDown = pSrc + side;
    *pDst++ = (1/2.0) * (*(pSrc+1) + *pDown++);
    pSrc++;
    for(j = 1; j < side-1; ++j) {
        *pDst++ = (1/3.0) * (*(pSrc-1) + *(pSrc+1) + *pDown++);
        pSrc++;
    }
    *pDst++ = (1/2.0) * (*(pSrc-1) + *pDown++);
    pSrc++;
    for(i = 1; i < side-1; ++i) {
        /* Handle left side */
        *pDst++ = (1/3.0) * (*pUp++ + *pDown++ + *(pSrc+1));
        for(j = 1; j < side-1; ++j) {

        }
    }
}
