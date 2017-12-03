
def mergePath(A,B):
    C = [0]*(len(A)+len(B))
    nbT = 5
    Ad = [0]*(nbT+1)
    Bd = [0]*(nbT+1)
    
    print(len(Ad),nbT)
    Ad[nbT] = len(A)
    Bd[nbT] = len(B)
    for i in range(1,nbT):
        index = i*(len(A)+len(B))/nbT
        atop = len(A)-1 if (index > len(A)) else index
        btop = index - len(A)+1 if (index > len(A)) else 0
        abot = btop
        print("while num ",i)
        while 1 :
            offset = (atop - abot)/2
            ai = atop - offset
            bi = btop + offset
            if bi <= 0:
                bi = 1
            if ai >= len(A):
                ai -= 1
            if( A[ai] > B[bi -1] ):
                if A[ai -1] <= B[bi]:
                    Ad[i] = ai
                    Bd[i] = bi
                    print("ai = ",ai," bi = ",bi,"offset",offset)
                    break
                else:
                    atop = ai - 1
                    btop = bi + 1

            else:
                abot = ai + 1
            print ai,bi
    size = (len(A)+len(B))/nbT
    #
    print Ad,Bd,"\n"
    print A,B, "\n"
    for i in range(1,nbT+1):
        otherMerge(A,Ad[i-1],Ad[i],B,Bd[i-1],Bd[i],C,i-1,size)
       
    #for i in range(0,nbT):
     #    merge(A,Ad[i],B,Bd[i],C,i,size)
    #  print C,A[Ad[i]],B[Bd[i]],size
      #  print A[Ad[i]:],B[Bd[i]:]
      #  print ""
    return C

def merge(A,ad,B,bd,C,pos,size):
    for i in range(0,size):
        if ad >= len(A):
            C[pos*size+i] = B[bd]
            bd += 1
        elif bd >= len(B):
            C[pos*size+i] = A[ad]
            ad += 1
        elif A[ad] > B[bd]:
            C[pos*size+i] = B[bd]
            bd += 1
        else:
            C[pos*size+i] = A[ad]
            ad += 1
    print C
def otherMerge(A,ab,ad,B,bb,bd,C,pos,size):
    print A[ab:ad],B[bb:bd]
    start = ab+bb
    i = 0
    while True:
        if ab == ad and bb == bd:
            print i,C
            return
        elif ab == ad:
            C[start+i] = B[bb]
            bb += 1
        elif bb == bd:
            C[start+i] = A[ab]
            ab += 1
        elif A[ab] > B[bb]:
            C[start+i] = B[bb]
            bb += 1
        else:
            C[start+i] = A[ab]
            ab += 1
        i += 1



A = [1,2,3,4,6,10,11,13]
B = [5,7,8,9,12,14,15,16]

print mergePath(A,B )
                   
