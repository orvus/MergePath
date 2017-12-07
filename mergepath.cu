#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NB 2
#define NTPB 4 
#define sizeTab
void testCUDA(cudaError_t error, const char *file, int line)  {
   if (error != cudaSuccess) {
      printf("There is an error in file %s at line %d : %d\n", file, line, error);
       exit(EXIT_FAILURE);
   } 
}

#define testCUDA(error) (testCUDA(error, __FILE__ , __LINE__))


__device__ int Ad[NB*NTPB+1];
__device__ int Bd[NB*NTPB+1];

__device__ void printRange(int idx,int* t, int d,int f){
   
   if(idx == 1){
       printf("\t[");
      for (int i = d; i < f; ++i)
      {
         printf("%d, ",t[i]);
      }
      printf("]\n");   
   }

      // printf("tab[%d:%d]\", d,f);
   
}


__device__ void merge(int idx,int* C,int* A, int ab,int ad,int* B, int bb, int bd, int pos,int size){
   int start = ab+bb;
   int i = 0;
  // printf("->%d\n", ad-ab + bd-bb);
   while(1){
      if(ab >= ad && bb >= bd){

        // printRange(idx,C,0,16);
         return;
      }else if(ab == ad){
         C[start+i] = B[bb];
         bb++;
      }else if(bb == bd){
         C[start+i] = A[ab];
         ab++;
      }else if( A[ab] > B[bb]){
         C[start+i] = B[bb];
         bb += 1; 
      }else{
         C[start+i] = A[ab];
         ab++;
      }
      i++;
   }
}



__device__ void mergePath(int* C,int* A,int lenA,int *B,int lenB,int nbT){
   
   int idx = (threadIdx.x + blockIdx.x * blockDim.x)% nbT +1;

   if(idx <= nbT){

      //C[idx-1] = idx;
      // Ad[0] = 0;
      // Bd[0] = 0;
      int test = 0;
      printf("\t\t%d  = idx (%d , %d et %d , %d\n",idx,A[0] ,A[1] , B[0], B[1]   );
      if(A[0] == 2 && A[1] == 9 && lenA == 2 && B[0] == 7 && B[1] == 10 ){
         test =1;
         printf("!!!!!!!!!!! %d\n",idx);
      }
      Ad[idx-1] = 0;
      Bd[idx-1] = 0;
      if(idx == 1){
      //    // printf("\n");
         Ad[nbT] = lenA;
         Bd[nbT] = lenB;
       
      
      }
      

      int index = (idx*(lenA+lenB))/(nbT);
      int atop = (index > lenA)? lenA-1: index;
      int btop = (index > lenA)?  index - (lenA-1) : 0;
      int abot = btop;
      //int i = 0;
      int offset;
      int ai;
      int bi;
      int flag = 0;
      while(!flag && idx != nbT){
         //printf("%d\n",i++ );
         offset = (int)floor(((float)(atop - abot))/(float)(2));
         ai = atop - offset;
         bi = btop + offset;
         if(test){
            printf("%d : (%d,%d) off: %d, atop %d btop %d abot %d\n %d > %d et %d <= %d ?  \n",idx,ai,bi,offset,atop,btop,abot,A[ai], B[bi-1] ,A[ai-1],B[bi]);
         }
         if( bi == 0 || ai >= lenA || A[ai] > B[bi-1]){
            if(A[ai-1] <= B[bi])
            {
              printf("%d : (%d,%d)\n",idx,ai,bi );
               Ad[idx] = ai;
               Bd[idx] = bi;
               flag = 1;
            }
            else{
               atop = ai - 1;
               btop = bi + 1;
            }
         }
         else{
            abot = ai + 1;
         }
      }
      __syncthreads();
         if(/*idx == 1 && */test){
            printf("plop\n");
         printRange(1,Ad,0,9);
         printRange(1,Bd,0,9);
      }
      int size = (lenA+lenB)/(nbT);
      for (int i = 1; i <= nbT; ++i)
      {
         merge(idx,C,A,Ad[i-1],Ad[i],B,Bd[i-1],Bd[i],i-1,size);
        // printRange(0,C,0,16);
      }
   }
}

__global__ void sort(int *R,int* S,int* T,int lenR){
   int idx = threadIdx.x + blockIdx.x * blockDim.x;
   int size = lenR/(NB*NTPB);
   int tmp;
   int turn = 1;
 //  printf("debut\n");
   if(2*lenR < NB*NTPB){
   //   printf("inf\n");
   }else{
     // printf("sup %d\n",size);

      for (int j = 0; j < size; j++)
      {
         for (int k = j; k < size; k++)
         {
            if( R[j+idx*size] > R[k+idx*size]){
               tmp = R[j+idx*size];
               R[j+idx*size] = R[k+idx*size];
               R[k+idx*size] = tmp;
            }
         }
      }
      for(int i = 0; i < size ; i++){
         S[idx*size+i] = R[idx*size+i];
            
      }
      printRange(idx,R,0,16);
      //fin premier etape
      __syncthreads();
      turn *=2;
      size *=2;
      //printf("fin premiere etape\n");
      while(size <= lenR){
         if( idx < lenR/(turn*2)){

            printf("%d -> mergePath(R+%d,S+%d ,%d, S+%d, %d,%d);\n",idx,idx*size,idx*size ,size/2,size/2+idx*size,size/2,1);
            printRange(idx,S,4,8);
            mergePath(R+idx*size,/*R+idx*size/2*/S+idx*size ,size/2, /*R+size/2+idx*size/2*/S+size/2+idx*size, size/2,1);
            __syncthreads();
            printRange(idx,R,4,8);
         }
         __syncthreads();
         // mergePath(R+idx*size,/*R+idx*size/2*/S+idx*size/2 ,size/2, /*R+size/2+idx*size/2*/S+size/2+idx*size/2, size/2,turn);
         for(int i = 0; i < size ; i++){
            S[idx*size+i] = R[idx*size+i];
            // T[idx*size+i] = R[idx*size+i];
            
         }
        // printf("traitement de : %d\n", size);
         turn *=2;
         size *=2 ;
         printRange(idx,R,0,16);
         __syncthreads();
      }
   }
}



void affiche(int* A,int len){
   for (int i = 0; i < len; ++i)
   {
      printf("%d, ",A[i] );
   }
   puts("\b\b  ");
}
#define s 512

int comp (const void * elem1, const void * elem2) 
{
    int f = *((int*)elem1);
    int s_ = *((int*)elem2);
    if (f > s_) return  1;
    if (f < s_) return -1;
    return 0;
}

int main(int argc,char** argv){
   // /*int A[s] = {5,17,11,19,18,15,16,114,119,120,14,112,117,159
   //                ,71,66,58,157,42,195,185,15,176,1124,1419
   //                ,120,147,112,1147,19,71,646
   //             };*/
   // // int C[2*s] = {0};
   // srand(time(NULL));
   // int* C = (int*)calloc(4*s,sizeof(int));
   // // int A[s] = {1,2,3,4,6,10,11,13};
   // // int B[s] = {5,7,8,9,12,14,15,16};
   // int *A = (int*)malloc(sizeof(int)*s);
   // // int *B = (int*)malloc(sizeof(int)*s);

   // int* ret = (int*)malloc(sizeof(int)*s);
   // for(int i =0 ; i < s ; i++){
   //    A[i] = rand()%1000+1;
   //    // B[i] = rand()%1000+1;
   //    ret[i] = 0;

   // }

   // int *aGPU,*bGPU,*cGPU;
   // cudaMalloc(&aGPU,s*sizeof(int));
   // cudaMemcpy(aGPU,A,s*sizeof(int),cudaMemcpyHostToDevice);
   
   // cudaMalloc(&bGPU,s*sizeof(int));
   // cudaMemcpy(bGPU,A,s*sizeof(int),cudaMemcpyHostToDevice);
   
   // cudaMalloc(&cGPU,2*s*sizeof(int));
   // cudaMemcpy(cGPU,A,s*sizeof(int),cudaMemcpyHostToDevice);
   // printf("debut :\n");
   // sort<<<NB,NTPB>>>(aGPU,bGPU,cGPU,s);

   // testCUDA(cudaMemcpy(ret,aGPU,s*sizeof(int),cudaMemcpyDeviceToHost));
   // affiche(ret,s);



   // qsort(A,s,sizeof(int),comp);

   // qsort(B,s,sizeof(int),comp);
   int t[16] = {7,5,1,4,9,2,7,10,11,20,3,8,12,30,17,18};
   affiche(t,16);
   int *tGPU,*uGPU,*vGPU;
   int *rettest = (int*)malloc(sizeof(int)*16);
   cudaMalloc(&tGPU,16*sizeof(int));
   cudaMemcpy(tGPU,t,16*sizeof(int),cudaMemcpyHostToDevice);
   cudaMalloc(&uGPU,16*sizeof(int));
   cudaMemcpy(uGPU,t,16*sizeof(int),cudaMemcpyHostToDevice);
   cudaMalloc(&vGPU,16*sizeof(int));
   cudaMemcpy(vGPU,t,16*sizeof(int),cudaMemcpyHostToDevice);
   

   sort<<<NB,NTPB>>>(tGPU,uGPU,vGPU,16);


   // for(int i = 0 ; i < 4; i++){
   //    mergePath<<<NB,NTPB>>>(tGPU+i*4,uGPU+i*4,2,vGPU+2+i*4,2,2); 
   //    cudaMemcpy(uGPU,tGPU,16*sizeof(int),cudaMemcpyDeviceToDevice);
   //    cudaMemcpy(vGPU,tGPU,16*sizeof(int),cudaMemcpyDeviceToDevice); 
   //    testCUDA(cudaMemcpy(rettest,tGPU,16*sizeof(int),cudaMemcpyDeviceToHost));
   //    affiche(rettest,16);
   // }
   //   for(int i = 0 ; i < 2; i++){
   //    mergePath<<<NB,NTPB>>>(tGPU+i*8,uGPU+i*8,4,vGPU+4+i*8,4,4); 
   //    cudaMemcpy(uGPU,tGPU,16*sizeof(int),cudaMemcpyDeviceToDevice);
   //    cudaMemcpy(vGPU,tGPU,16*sizeof(int),cudaMemcpyDeviceToDevice); 
   //    testCUDA(cudaMemcpy(rettest,tGPU,16*sizeof(int),cudaMemcpyDeviceToHost));
   //    affiche(rettest,16);
   // }
   
   
   testCUDA(cudaMemcpy(rettest,tGPU,16*sizeof(int),cudaMemcpyDeviceToHost));
   affiche(rettest,16);
}


// __global__ void merge(int* A,int lenTab){
//    int idx = threadIdx.x + blockIdx.x * blockDim.x;
//    int bidx = threadIdx.x;
//    int turn = 1;
//    int end = 0;
//    int size = lenTab/(NB*NTPB);
//    if(size < 2){
//       size = 2;
//    }
//    __shared__ int *sT;
//    //premier tri de size element

//    //tri A[idx*size : (idx+1)*size-1]
//    for (int i = 0; i < size; i++)
//    {
//       sT[bidx+i] = A[idx+i];

//    }
//    for (int j = 0; j < size; j++)
//    {
//       for (int k = j; k < size; k++)
//       {
//          if( sT[j] > sT[k]){
//             int tmp = sT[j];
//             sT[j] = sT[k];
//             sT[k] = sT[j];
//          }
//       }
//    }
//    turn++;
//    size *=2;
//    for (int i = 0; i < size; i++)
//    {
//       // sT[bidx+i] = A[idx+i];
//       A[idx] = sT[bidx];
//    }
//    // while(!end){

//    // }


//    //suite 


// }


// void mergepath(int* C,int *A,int lenA,int* B, int lenB,int nbT){
//    int Adiag[nbT+1] = {0}
//    int Bdiag[nbT+1] = {0}

//    Adiag[nbT] = lenA
//    Bdiag[nbT] = lenB
//    for( int i = 0; )
// }