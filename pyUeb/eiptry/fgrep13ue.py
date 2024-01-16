
def fgrep(subject: str, filename: str, v: bool, i: bool):
    with open(filename.txt.rtf) as f: 
        for n, line in enumerate(f):
            c = subject.lower() in line.lower() if i else subject in line 
            if c != v:
                yield (n, line)