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

    def Viterbi(self, query):
        """
        Output: 
        The resulting log-likelihood score as well as the alignment 
        of the query sequence to the profile HMM based on this path 
        """
        Vscore = 0
        aln = ""

        """
        matrix is in following form:
        -   B   M   I   D   E
        x1:
        x2:
        x3:
        ...

        """


        """
        for each row in dp matrix, for Mi set to max from M_i-1, I_i-1, D_i-1
        same for D, for I_i set to max from I_i, M_i, D_i"""
        #if we are in a M,I,D state, use this value to tell which one
        M_index = 1
        I_index = 0
        D_index = 1

        #start state is M0
        print(len(query))
        dp_matrix = [[float('-inf'), float('-inf'), float('-inf')] for _ in range(len(query)+1)]
        dp_matrix[0][0] = 0
        print(f"182 {dp_matrix}")
        #New value is max of 3x Transition+emission
        # query is a string of A/C/G/T
        for char_ind in range(len(query)):
            char = query[char_ind]
            for index in range(self.nstate):
                print(f"188char_ind {char_ind}")
                print(f"189index {index}")
                #find value for M 
                dp_matrix[char_ind+1][0] = self.eM[index+1][char] + max( 
                    #take max of log likelihood(ll) for each location in row above +
                    #transition ll to M in current row + ll of emission of char in M
                    dp_matrix[char_ind][0] + self.t[index]['MM'],
                    dp_matrix[char_ind][1] + self.t[index]['IM'],
                    dp_matrix[char_ind][2] + self.t[index]['DM']
                )
                #after transitioning to an M state, the M index goes up by 1
                #find value for I
                dp_matrix[char_ind+1][1] = self.eI[index][char] + max (
                    #take max of ll for each location in row above + transition to I +emission of char
                    dp_matrix[char_ind][0] + self.t[index]['MI'],
                    dp_matrix[char_ind][1] + self.t[index]['II'],
                    dp_matrix[char_ind][2] + self.t[index]['DI']
                )
                #find value for D
                dp_matrix[char_ind+1][2] = max (
                    #take max of ll for each location in row above + transition to I, D does not emit
                    dp_matrix[char_ind][0] + self.t[index]['MD'],
                    dp_matrix[char_ind][1] + self.t[index]['ID'],
                    dp_matrix[char_ind][2] + self.t[index]['DD']
                )
            print(f"213 {dp_matrix}")
        # Add implementation here!
        last_row = len(query)
        Vscore = max(dp_matrix[last_row][0], dp_matrix[last_row][1], dp_matrix[last_row][2])
        aln = 'A'
        return Vscore, aln
