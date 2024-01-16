#!/usr/bin/env python3

import sys


class BFError(Exception):
    pass


def bf(sourcefn, infn=None, outfn=None):
    """ brainfuck main interpreter function
        Arguments:
        sourcefn - filename of program
        infn     - filename of input file (or False)
        outfn    - filename of output file (or False)

    """

    try:
        fout = None
        (src, fin, fout) = open_files(sourcefn, infn, outfn)
        bfinterpret(src.read(), fin, fout)
    except IOError as e:
        print("I/O-Fehler:", e)
    except BFError as e:
        print("Abbruch wegen BF-Inkompatibilität:", e)
    except Exception as e:
        print("Interner Interpreter-Fehler:", e)
    finally:
        if fout:
            fout.close()


def open_files(srcfn, infn, outfn):
    """open_files opens all necessary files and returns file objects
    returns triple of file objects

    Arguments:
    srcfn - filname of source file
    infn - filename of input file, if empty then no input
    outfn - filename of output file, if empty then output to console

    """

    fin  = open(infn)       if infn  else sys.stdin
    fout = open(outfn, "w") if outfn else sys.stdout
    return(open(srcfn), fin, fout)


def bfinterpret(srctext, fin=sys.stdin, fout=sys.stdout):
    """Interprets a BF program

    Arguments:
    srctext - the BF source
    fin - an input file object
    fout - an output file object
    """
    pc = 0
    ptr = 0
    data = dict()
    while (pc < len(srctext)):
        (pc, ptr) = instr.get(srctext[pc], noop)(pc, ptr,
                                                 srctext, data,
                                                 fin, fout)
        pc += 1


def noop(pc, ptr, src, data, fin, fout):
    return(pc, ptr)


def left(pc, ptr, src, data, fin, fout):
    return(pc, ptr - 1)


def right(pc, ptr, src, data, fin, fout):
    return(pc, ptr + 1)


def incr(pc, ptr, src, data, fin, fout):
    data[ptr] = data.get(ptr, 0) + 1
    return(pc, ptr)


def decr(pc, ptr, src, data, fin, fout):
    data[ptr] = data.get(ptr, 0) - 1
    return(pc, ptr)


def beginloop(pc, ptr, src, data, fin, fout):
    if data.get(ptr, 0):
        return (pc, ptr)
    loop = 1
    while loop > 0:
        pc += 1
        if pc >= len(src):
            raise BFError("Kein ']' gefunden")
        if src[pc] == ']':
            loop -= 1
        elif src[pc] == '[':
            loop += 1
    return(pc, ptr)


def endloop(pc, ptr, src, data, fin, fout):
    loop = 1
    while loop > 0:
        pc -= 1
        if pc < 0:
            raise BFError("Kein '[' gefunden")
        if src[pc] == ']':
            loop += 1
        elif src[pc] == '[':
            loop -= 1
    return(pc - 1, ptr)


def ch_in(pc, ptr, src, data, fin, fout):
    ch = fin.read(1)
    if ch:
        data[ptr] = ord(ch)
        if data[ptr] > 127:
            raise BFError("Non-ASCII-Zeichen gelesen")
    return(pc, ptr)


def ch_out(pc, ptr, src, data, fin, fout):
    if data.get(ptr, 0) > 127:
        BFError("Ausgabe eines Non-ASCII-Zeichen")
    print(chr(data.get(ptr, 0)), end='', file=fout, flush=True)
    return(pc, ptr)


instr = {'<': left, '>': right, '+': incr, '-': decr,
         '.': ch_out, ',': ch_in, '[': beginloop, ']': endloop}


if __name__ == "__main__":
    if len(sys.argv) > 2:
        infile = sys.argv[2]
    else:
        infile = None
    if len(sys.argv) > 3:
        outfile = sys.argv[3]
    else:
        outfile = None

    try:
        bf(sys.argv[1], infile, outfile)
    except IndexError:
        print("Kein BF Programm angegeben")
