
def fgrep2 (subject : str, infile :str, outfile :str):
    with open(infile) as fin, open (outfile, 'w') as fout:
        for line in fin:
            if not (subject in line):
                print(line, file= fout)
                


def fgrep (subject : str, filename :str):
    with open(filename) as f:
        for line in f:
            if subject in line:
                print(line)

