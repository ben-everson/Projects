from math import *
import sys


INF = float("inf")


class HMM:
    def __init__(self):
        self.alphabet = []
        self.eI = []      # background probability of each character in the alphabet
        self.eM = [{}]    # emission probability, one dictionary for each matching state in the model
        self.t = []       # transition probability, one dictionary for each set of states (D,M,I) in the model
        self.nstate = 0
    
    def load(self,hmmfile):
    # only load the first model in the given hmmfile if there are two or more models
        with open(hmmfile,'r') as fin:
            for line in fin:
                stream = line.strip().split()

                if stream[0] == "LENG":
                    self.nstate = int(stream[1])

                if stream[0] == "HMM": 
                    # read alphabet
                    self.alphabet = stream[1:]
                    
                    # read transition order
                    stream = fin.readline().strip().split()
                    trans_order = [(y[0]+y[3]).upper() for y in stream]
                    
                    # read the next line, if it is the COMPO line then ignore it and read one more line
                    stream = fin.readline().strip().split()
                    if stream[0] == "COMPO":
                        stream = fin.readline().strip().split()
                    
                    # now the stream should be at the I0 state; read the emission of the I0 state 
                    e = {}
                    for (x,y) in zip(self.alphabet,stream):
                        e[x] = -float(y)
                    self.eI.append(e)    
                    
                    # now the stream should be at the B state; read in the transition probability
                    stream = fin.readline().strip().split()
                    tB = {'MM':-INF,
                          'MD':-INF,
                          'MI':-INF,
                          'IM':-INF,
                          'II':-INF,
                          'ID':-INF,
                          'DM':-INF,
                          'DI':-INF,
                          'DD':-INF}
                    for x,y in zip(trans_order,stream):
                        tB[x] = -INF if y == '*' else -float(y)

                    self.t.append(tB)
                    break    

            for i in range(1, self.nstate+1):
                # read each set of three lines at a time
                stream = fin.readline().strip().split() # this one is the emission of the M state
                if float(stream[0]) != i:
                    print("Warning: incosistent state idexing in hmm file; expecting state " + str(i) + "; getting state " + stream[0])
                e = {}
                for x,y in zip(self.alphabet,stream[1:]):
                    e[x] = -INF if y == "*" else -float(y) 
                self.eM.append(e)    

                # the next line is the emission of the I state
                stream = fin.readline().strip().split() 
                e = {}
                for x,y in zip(self.alphabet,stream):
                    e[x] = -INF if y == "*" else -float(y) 
                self.eI.append(e)                    

                # the next line contains the transition probs
                stream = fin.readline().strip().split() # this one is the transition prob
                tB = {'MM':-INF,
                      'MD':-INF,
                      'MI':-INF,
                      'IM':-INF,
                      'II':-INF,
                      'ID':-INF,
                      'DM':-INF,
                      'DI':-INF,
                      'DD':-INF}
                for x,y in zip(trans_order,stream):
                    tB[x] = -INF if y == '*' else -float(y)
                self.t.append(tB)      

    def compute_llh(self, query):
        # Compute likelihood of an aligned query to the HMM
        # Return -inf if the query is not properly aligned
        j = 0
        prev_state = 'M'
        llh = 0

        for c in query:
            if c == '-': # gap -> deletion
                curr_state = 'D'
            elif c >= 'a' and c <= 'z': # lowercase -> insertion
                curr_state = 'I'
            elif c >= 'A' and c <= 'Z': # uppercase -> match
                curr_state = 'M'
            else: # encounter invalid symbol
                #print("encountered invalid symbol!")
                return -INF

            trans = prev_state + curr_state

            # penalize the transition
            if trans in self.t[j]:
                #print(self.t[j][trans])
                llh += self.t[j][trans]
            else: # encounter invalid transition
                #print("invalid transition  " + trans)
                return -INF 
            
            # transit: update the state index
            j += (curr_state != 'I') # move to the nex state unless this is moving towards an 'I'
            if j > self.nstate: # reach end of the HMM chain but not end of the sequence
                #print("reach end of the HMM chain but not end of the sequence")
                return -INF

            # penalize the emission
            if curr_state == 'M':
                #print(self.eM[j][c])
                llh += self.eM[j][c]
            elif curr_state == 'I': 
                #print(self.eI[j][c.upper()])   
                llh += self.eI[j][c.upper()]
            
            # update state
            prev_state = curr_state

        if j != self.nstate: # does not reach the E state at the end
            #print("does not reach the E state at the end")
            return float("-inf")
        trans = prev_state + 'M'
        llh += self.t[j][trans]    
        return llh

    #takes in query of input sequence and outputs most probable alignment to the MSA as
    #given by the maximum ll/vscore
    def Viterbi(self, query):
        """
        Full-matrix Viterbi with traceback for a Profile HMM.
        Returns: (log-likelihood-score (ll), alignment_string)
        alignment_string uses:
            uppercase letters for Match state emissions
            lowercase letters for Insertion emissions
            '-' for Deletions
        """

        NEG_INF = -float("inf")
        L = len(query) # set L to be number of nucleotides in sequence
        K = self.nstate # set K to be number of match states in model

        # create DP matrices with extra row for starting state and col for ending/starting state
        M = [[NEG_INF] * (K + 1) for _ in range(L + 1)]
        I = [[NEG_INF] * (K + 1) for _ in range(L + 1)]
        D = [[NEG_INF] * (K + 1) for _ in range(L + 1)]
        print(f"M:{M}")
        print(f"I:{I}")
        print(f"D:{D}")

        # Backpointers store: (prev_state, prev_i, prev_k)
        # prev_state in {'M','I','D', None}
        bM = [[None] * (K + 1) for _ in range(L + 1)]
        bI = [[None] * (K + 1) for _ in range(L + 1)]
        bD = [[None] * (K + 1) for _ in range(L + 1)]

        # Start at (i=0,k=0) in M0 with score 0
        M[0][0] = 0.0
        bM[0][0] = (None, -1, -1) #does not point back to any other state

        #initialize D to allow for deletions to be first state M0 -> D1 -> D2 -> M3
        # D[0][k] = max( M[0][k-1] + t[k-1]['MD'], D[0][k-1] + t[k-1]['DD'], I[0][k-1]+t[k-1]['ID'] 
        for k in range(1, K + 1):
            candidates = [
                #store backpointer with candidate
                (M[0][k - 1] + self.t[k - 1]['MD'], ('M', 0, k - 1)),
                (D[0][k - 1] + self.t[k - 1]['DD'], ('D', 0, k - 1)),
                (I[0][k - 1] + self.t[k - 1]['ID'], ('I', 0, k - 1))
            ]
            #find max log likelihood then store it in best_val and store backpointer
            best_val, best_ptr = max(candidates, key=lambda p: p[0])
            D[0][k] = best_val
            bD[0][k] = best_ptr

        # fill DP matrix
        for index in range(1, L + 1): #for each char in input sequence
            ch = query[index - 1]

            # k=0: only I0 can emit while staying at model position 0
            # I[i][0] = eI[0](ch) + max( M[i-1][0]+t[0]['MI'], I[i-1][0]+t[0]['II'], (optionally D[i-1][0]+t[0]['DI']) )
            emitI0 = self.eI[0][ch] # get ll to emit ch from I)
            cand_I0 = [
                (M[index - 1][0] + self.t[0]['MI'], ('M', index - 1, 0)),
                (I[index - 1][0] + self.t[0]['II'], ('I', index - 1, 0)),
                (D[index - 1][0] + self.t[0]['DI'], ('D', index - 1, 0))
            ]

            #find most probable prior location
            best_base, best_ptr = max(cand_I0, key=lambda p: p[0]) 
            I[index][0] = emitI0 + best_base
            bI[index][0] = best_ptr

            # Cant get to M[i][0] and D[i][0] -would need to increase index
            M[index][0] = NEG_INF
            D[index][0] = NEG_INF

            #iterate through each state of the model
            for k in range(1, K + 1):
                
                #Match states
                cand_M = [
                    #Prev value + transition ll
                    (M[index - 1][k - 1] + self.t[k - 1]['MM'], ('M', index - 1, k - 1)),
                    (I[index - 1][k - 1] + self.t[k - 1]['IM'], ('I', index - 1, k - 1)),
                    (D[index - 1][k - 1] + self.t[k - 1]['DM'], ('D', index - 1, k - 1)),
                ]
                best_base, best_ptr = max(cand_M, key=lambda p: p[0])
                M[index][k] = self.eM[k][ch] + best_base #add emission prob of char
                bM[index][k] = best_ptr

                #Insertion States
                cand_I = [
                    (M[index - 1][k] + self.t[k]['MI'], ('M', index - 1, k)),
                    (I[index - 1][k] + self.t[k]['II'], ('I', index - 1, k)),
                    (D[index - 1][k] + self.t[k]['DI'], ('D', index - 1, k))
                ]
                best_base, best_ptr = max(cand_I, key=lambda p: p[0])
                I[index][k] = self.eI[k][ch] + best_base #add emmision pro of I
                bI[index][k] = best_ptr #update for backtrackng

                # Deletion States
                cand_D = [
                    (M[index][k - 1] + self.t[k - 1]['MD'], ('M', index, k - 1)),
                    (I[index][k - 1] + self.t[k - 1]['ID'], ('I', index, k - 1)),
                    (D[index][k - 1] + self.t[k - 1]['DD'], ('D', index, k - 1)),

                ]
                best_val, best_ptr = max(cand_D, key=lambda p: p[0])
                D[index][k] = best_val #no emmission prob for deletion state
                bD[index][k] = best_ptr


        # compute final VScore and get backpointer
        end_cands = [
            #ll to be in final location + transition to end state
            (M[L][K] + self.t[K]["MM"], ('M', L, K)),
            (I[L][K] + self.t[K]["IM"], ('I', L, K)),
            (D[L][K] + self.t[K]["DM"], ('D', L, K)),
        ]
        Vscore, (state, index, k) = max(end_cands, key=lambda p: p[0])

        # --- Traceback ---
        aln = [] #store alignment while iterating through pointer
        while True:
            if state is None or (index==0 and k == 0):
                break
            #If state is a match state, add upper char
            if state == 'M':
                prev = bM[index][k] #get prev pointer
                aln = [query[index-1]] + aln
                state, index, k = prev
            #If state is Insertion, change char to lower
            elif state == 'I':
                prev = bI[index][k]
                aln = [query[index - 1].lower()] + aln
                state, index, k = prev
            #If state is deletion, add a -
            elif state == 'D':
                prev = bD[index][k]
                aln = ['-'] + aln
                state, index, k = prev

            else:
                print("ERROR: bad pointers: 279")

        return Vscore, "".join(aln)