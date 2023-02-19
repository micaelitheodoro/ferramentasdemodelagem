#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#define N 1000
#define k 40
//#define p 0.5
#define max k*(k-1)/2
#define NUM 20

void Gerando(int k2,int M[N][N]);
void impressao(int M[N][N]);
void quebra(int k2,int M[N][N],float p);
float cluster(int M[N][N]);
float caminho(int M[N][N]);
main(){

  int k2,M[N][N],w;
  float coef,cam,soma1,soma2,p,coef0,cam0,y;
  FILE *arq;
  printf("ok\n");
  arq=fopen("rede1.dat","w");
  k2=k/2;
  soma1=soma2=0.;
  
  srand(1188713);
  Gerando(k2,M);
rintf("ok\n");
  // impressao(M);
  coef0=cluster(M);
  printf("0=%f\n",coef0);
  cam0=caminho(M);
  
  
  for(y=0.;y<=1.0;y=y+1.0){
   
    p=pow(10,-y);
    soma1=0.0;
    soma2=0.0;
    for(w=0;w<NUM;w++){
      printf("ok1\n");
      quebra(k2,M,p);
      // impressao(M);
      //cluster(M);
      coef=cluster(M);
      soma1=soma1+coef;
      cam=caminho(M);
      soma2=soma2+cam;
      Gerando(k2,M);
      
    }
    printf("s=%f %f\n",soma1,soma2);
    soma1=soma1/(NUM*coef0);
    soma2=soma2/(NUM*cam0);
    fprintf(arq,"%f %f %f\n",-y,soma1,soma2);

  }
  fclose(arq);
}
void Gerando(int k2,int M[N][N])
{
  int i,j;
  for(i=0;i<N;i++){
    for(j=0;j<N;j++){
      M[i][j]=0;
    }
  }
  //preenche em torno das diagonais
  for(i=0;i<N-k2;i++){
    for(j=i+1;j<=i+k2;j++){
      M[i][j]=M[j][i]=1;
    }
  }
  for(i=N-k2;i<N-1;i++){
    for(j=i+1;j<N;j++){
      M[i][j]=M[j][i]=1;
    }
  }
  //preenche as pontas
  for(i=0;i<k2;i++){
    for(j=N-1;j>=N-k2+i;j--){
      M[i][j]=M[j][i]=1;
    }
  }
}
/*
void impressao(int M[N][N]){
  int i,j;
  for(i=0;i<N;i++){
    for(j=0;j<N;j++){
      //printf("%d ",M[i][j]);
    }
    // printf("\n");
  }
  }*/
void quebra(int k2,int M[N][N],float p){
 
  int i,j,aux,l;
  float xe;
  for(aux=1;aux<=k2;aux++){
    
    //quebra em torno das diagonais
    for(i=0;i<N-aux;i++){
      j=i+aux;
      xe=(float)(rand()/(RAND_MAX+1.));
      if(xe<=p){//testa se ha quebra
	l=N*(rand()/(RAND_MAX+1.));
	if(l!=i){//sorteia com qual no sera
	  // refeita a ligacao
	  if(M[i][l]!=1){
	    M[i][l]=M[l][i]=1;
	    M[i][j]=M[j][i]=0;
	  }
	}
      }
    }
    
    //quebra as pontas
    for(i=0;i<aux;i++){
      j=N-aux+i;
      xe=(float)(rand()/(RAND_MAX+1.));
      if(xe<=p){//testa se ha quebra
	l=N*(rand()/(RAND_MAX+1.));
	if(l!=i){//sorteia com qual no sera 
	  //refeita a ligacao
	  if(M[i][l]!=1){
	    M[i][l]=M[l][i]=1;
	    M[i][j]=M[j][i]=0;
	  }
	}
      }
    }
  }
  
}

float cluster(int M[N][N]){

  int V[max],E,aux1,kk,j,i;
  float coef;
  E=0;
  
  //roda as linhas
  for(i=0;i<N;i++){
    kk=0;
    for(j=0;j<N;j++){
      if(M[i][j]){
	V[kk]=j; //guarda as posicoes
	kk++;
	
      }
    }
    for(j=0;j<kk;j++){
      for(aux1=j+1;aux1<kk;aux1++){
	if(M[V[j]][V[aux1]]) E++;
      }
    }
  }

  coef=(float)E/(N*max*2);
  return(coef);
}

float caminho(int M[N][N]){
  int i,j,kk,V[N],aux,a,numero,aux3,aux2,aux4,w;
  float soma;
  soma=0.;
  for(i=0;i<N;i++){
    V[0]=i;
    aux=aux2=aux3=aux4=0;
    numero=1;
   
    for(kk=0;kk<=aux;){//roda o vetor V
      
      if(aux2<numero){//aux2=numero de elementos
	// olhados do nivel
	for(j=0;j<N;j++){
	  if(M[V[kk]][j]){
	    a=1;
	    for(w=0;w<=aux;w++){//verifica o vetor V
	      if(V[w]==j){
		a=0;
		break;
	      }
	    }
	    if(a){
	      aux++;//aumenta o vetor V
	      aux4++;//guarda o numero de 
	      //elementos do proximo nivel
	      V[aux]=j;
	    }
	  }
	}
	aux2++;
	kk++;
      }//fecha if
      else{
	soma=soma+aux3*aux4;//faz a soma dos caminhos
	numero=aux4;//substitui o numero de 
	//elementos do proximo nivel
	aux4=0;
	aux3++;//aumenta um caminho
	aux2=0;

      }
    }//fecha kk
      
  }//fecha i
  soma=soma/max;
  return(soma);
}
