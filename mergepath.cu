#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NB 1
#define NTPB 1024

#define SIZE 2048*512


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

   
}


/*------ @param : int 
         idx   : indice du thread qui entre dans la fonction
         C     : tableau de résultat 
         A     : tableau d'entrée
         ab    : indice de debut pour A  
         ad    : indice de fin pour A
         B     : tableau d'entrée
         bb    : indice de début pour B
         bd    : indide de fin pour B


*/
__device__ void merge(int idx,int* C,int* A, int ab,int ad,int* B, int bb, int bd){
   int start = ab+bb;
   int i = 0;
   while(1){
      if(ab >= ad && bb >= bd){

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


/*------ @param : 
            C : fusion de A et B trié
            A : tableau d' entrée trié de lenR éléments
            B : tableau d' entrée trié de lenR éléments
            len* : longeur du talbeau *
            nbT : nombre de thread dans la fonction 

*/
__device__ void mergePath(int* C,int* A,int lenA,int *B,int lenB,int nbT){
   
   int idx = (threadIdx.x + blockIdx.x * blockDim.x)% nbT +1;

   if(idx <= nbT){
 
      Ad[idx-1] = 0;
      Bd[idx-1] = 0;
      if(idx == 1){
         Ad[nbT] = lenA;
         Bd[nbT] = lenB;
       
      
      }
      

      int index = (idx*(lenA+lenB))/(nbT);
      int atop = (index > lenA)? lenA-1: index;
      int btop = (index > lenA)?  index - (lenA-1) : 0;
      int abot = btop;
      int offset;
      int ai;
      int bi;
      int flag = 0;
      while(!flag && idx != nbT){
         offset = (int)floor(((float)(atop - abot))/(float)(2));
         ai = atop - offset;
         bi = btop + offset;
        
         if( bi == 0 || ai >= lenA || A[ai] > B[bi-1]){
            if(A[ai-1] <= B[bi])
            {
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
     // int size = (lenA+lenB)/(nbT);
      for (int i = 1; i <= nbT; ++i)
      {
         merge(idx,C,A,Ad[i-1],Ad[i],B,Bd[i-1],Bd[i]);
      }
   }
}


__device__ void mergeTab(int* C,int* A,int a,int* B,int b)
{
   int start = 0;
   int i = 0;
   int a_ = 0,b_ = 0;
   while(1){
      if(a_ >= a && b_ >= b){
         return;
      }else if(a_ == a){
         C[start+i] = B[b_];
         b_++;
      }else if(b_ == b){
         C[start+i] = A[a_];
         a_++;
      }else if( A[a_] > B[b_]){
         C[start+i] = B[b_];
         b_ += 1; 
      }else{
         C[start+i] = A[a_];
         a_++;
      }
      i++;
   }
}


/*------ @param : 
            R : tableau de sortie trié de lenR éléments
            S : tableau d' entrée trié de lenR éléments
            lenR : longeur des tableaux

*/
__global__ void sort(int *R,int* S,int lenR){
   int idx = threadIdx.x + blockIdx.x * blockDim.x;
   int size = lenR/(NB*NTPB);
   int tmp;
   int turn = 1;
   if(lenR < 2*NB*NTPB){
      size = 2;
   }
   printf("%d : size\n",size );
   //création de tableau trié de "size" éléments
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
   //optimisation ?
   // for(int l = 2 ; l <= size; l *= 2){
   //    for(int m = l ; m <= size ; m *=2){
   //       printf("%d,%d -> mergePath(R+%d,S+%d ,%d, S+%d, %d,%d);\n",idx,m,idx*m,idx*m ,l/2,l/2+idx*m,l/2,1);
   //       mergeTab(R+idx*m,S+idx*m,l/2,S+l/2+idx*m,l/2);   
   //    }
      
   //    for(int i = 0; i < size ; i++){
   //       S[idx*size+i] = R[idx*size+i];

   //    }
   //    // memcpy(S,R,menR);

   // }
  printRange(idx,R,0,lenR);


   for(int i = 0; i < size ; i++){
      S[idx*size+i] = R[idx*size+i];
         
   }
   // memcpy(S+idx*size,R+idx*size,size);
   //__syncthreads();
   turn *=2;
   size *=2;
   idx = idx%((NB*NTPB)/turn);
   while(size <= lenR){
     //printf("%d,%d -> mergePath(R+%d,S+%d ,%d, S+%d, %d,%d);\n",idx,turn,idx*size,idx*size ,size/2,size/2+idx*size,size/2,1);
      mergePath(R+idx*size, S+idx*size ,size/2, S+size/2+idx*size, size/2,turn);
      
      for(int i = 0; i < size ; i++){
         S[idx*size+i] = R[idx*size+i];
         
      }
      // memcpy(S+idx*size,R+idx*size,size);

      turn *=2;
      idx = idx%((NB*NTPB)/turn);
      size *=2 ;

   }
   
}


//affiche un tablrau de "len" élément(s)
void affiche(int* A,int len){
   for (int i = 0; i < len; ++i)
   {
      printf("%d, ",A[i] );
   }
   puts("\b\b  ");
}


int comp (const void * elem1, const void * elem2) 
{
    int f = *((int*)elem1);
    int s_ = *((int*)elem2);
    if (f > s_) return  1;
    if (f < s_) return -1;
    return 0;
}

int main(int argc,char** argv){

   // Initialisation des tableau
   int *A = (int*)malloc(sizeof(int)*SIZE);
   int* ret = (int*)malloc(sizeof(int)*SIZE);

   // srand(time(NULL));
   for(int i =0 ; i < SIZE ; i++){
      A[i] = rand()%10000+1;
      ret[i] = 0;

   }
   //---------------------------------

   //création des variable sur le GPU
   int *aGPU,*bGPU;
   testCUDA(cudaMalloc(&aGPU,SIZE*sizeof(int)));
   testCUDA(cudaMemcpy(aGPU,A,SIZE*sizeof(int),cudaMemcpyHostToDevice));
   testCUDA(cudaMalloc(&bGPU,SIZE*sizeof(int)));
   testCUDA(cudaMemcpy(bGPU,A,SIZE*sizeof(int),cudaMemcpyHostToDevice));
   //---------------------------------
   affiche(A,SIZE); // affichage du tableau non trié
   printf("len tab : %d\n",SIZE );

   // gestion du temps 
   int count;
   cudaDeviceProp prop;
   testCUDA(cudaGetDeviceCount(&count));
   testCUDA(cudaGetDeviceProperties(&prop, count-1));
   
   float TimerAddOne;                        // GPU timer instructions
   cudaEvent_t start, stop;                  // GPU timer instructions
   testCUDA(cudaEventCreate(&start));           // GPU timer instructions
   testCUDA(cudaEventCreate(&stop));            // GPU timer instructions
   testCUDA(cudaEventRecord(start,0));          // GPU timer instructions
   //-----------------


   // lancement de  l'algorithme de tri
   sort<<<NB,NTPB>>>(aGPU,bGPU,SIZE);


   // récupération du temps mis par le GPU
   testCUDA(cudaEventRecord(stop,0));           // GPU timer instructions
   testCUDA(cudaEventSynchronize(stop));        // GPU timer instructions
   testCUDA(cudaEventElapsedTime(&TimerAddOne,     // GPU timer instructions
          start, stop));                     // GPU timer instructions


   // récupération du talbeau sur le CPU
   testCUDA(cudaMemcpy(ret,aGPU,SIZE*sizeof(int),cudaMemcpyDeviceToHost));
   printf("\n\n");
   affiche(ret,SIZE);
   printf("GPU Timer for the addition on the GPU of vectors: %f ms\n", 
         TimerAddOne);

   // libération de la memoire
   free(A);free(ret);
   testCUDA(cudaFree(aGPU));
   testCUDA(cudaFree(bGPU));
}

