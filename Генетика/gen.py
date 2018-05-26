import shutil
import os
import subprocess
import glob
import re
import time

start_time = time.time()

if 'fp' in os.listdir('/Users/ivan/Desktop/'):
    shutil.rmtree('/Users/ivan/Desktop/fp')
os.mkdir('/Users/ivan/Desktop/fp')

if 'res' in os.listdir('/Users/ivan/Desktop/'):
    shutil.rmtree('/Users/ivan/Desktop/res')
os.mkdir('/Users/ivan/Desktop/res')

testsDir = '/Users/ivan/Desktop/fp/'
racketDir = '/Applications/Racket v6.11/bin/racket'
resultsDir = '/Users/ivan/Desktop/res/'

#print(os.listdir('/Users/ivan/Desktop/'))



subprocess.call([racketDir, '/Users/ivan/Desktop/tests.rkt'])

tests = glob.glob(testsDir + '*.in')

for test in tests:
    print(test)
    out = subprocess.Popen([racketDir, '/Users/ivan/Desktop/gen_in_progress.rkt', test],
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    output, err = out.communicate()
    print(err)
    print(output)
    #output = out.stdout
    #print(out)
    output = output.decode("utf-8").split("\n")
    #print(output)
    #for line in output:
    #    print('wot' + line)
    with open(resultsDir + test.split('/')[-1].replace('in', 'out'), 'w') as fil:
        for line in output:
            print(line, file=fil)





results = glob.glob(resultsDir + '*.out')
final = open('/Users/ivan/Desktop/res/aaa_res','w')
for result in results:
    with open(testsDir + result.split('/')[-1],'r') as fil, open(result,'r') as res:
        print(fil)
        print(res)
        #print(fil.readline() == res.readline())
        a = res.readline()
        b = fil.readline()
        if a == b:
            print(a)
            print(b)
            print('test ' + result.split('/')[-1])
            print()
            if a == '#f\n' or b == '#f\n':
                final.write('test ' + result.split('/')[-1] + ' passed' + '\n')
            else:
                if res.readline() == fil.readline():
                    final.write('test ' + result.split('/')[-1] + ' passed' + '\n')
                else:
                    final.write('test ' + result.split('/')[-1] + ' wrong paint'+'\n')
        else:
            final.write('test ' + result.split('/')[-1] + ' wrong answer'+'\n')


print("--- %s seconds ---" % (time.time() - start_time))



#f = open('/Users/ivan/Desktop/hello.out')
#r = open('/Users/ivan/Desktop/bye.in')

def check(f,r):
    f.readline()
    tmp = int(f.readline())
    graph = r.readline()
    str = f.readline()
    a = re.findall("\w+",str)
    c = {a[i] : a[i+1] for i in range(0,len(a)-1,2)}# словарь с раскрасками (Node2: 1, где 1 это номер цвета)
    b = re.sub('\("\w+"\)', '', graph)# отсекаем несвязные выршины графа
    b = re.findall("\w+",b)# список, в котором две соседние вершины это ребро графа
    print(b)
    print(c)
    if tmp != len(set(c.values())):
        return False
    for i in range(0,len(b) - 1,2):
        #print(c.get(b[i]), ' ' , c.get(b[i+1]))
        if c.get(b[i]) == c.get(b[i+1]):
            return False
    return True
#print(check(f,r))




