#first download sequences from link: https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Nucleotide&VirusLineage_ss=Severe%20acute%20respiratory%20syndrome%20coronavirus%202,%20taxid:2697049&utm_source=data-hub

from Bio import SeqIO
import sys, os

outdir = sys.argv[1]
steps = sys.argv[2]
files = sys.argv[3:]


seqs = []
for fname in files:
    a = SeqIO.parse(fname, 'fasta')
    for l in a:
        # with open(os.path.join(outdir, l.id + '.fasta'), 'w') as out:
            # out.write(''.join([c for c in str(l.seq).upper() if c in ['A', 'T', 'C', 'G']]))
        seqs.append(''.join([c for c in str(l.seq).upper() if c in ['A', 'T', 'C', 'G']]))
        
steps = range(1, len(seqs) + 1) if steps == "--all" else [int(x) for x in steps.split(',')]

for num in steps:
    curseq = ''.join(seqs[:num])
    with open(outdir + '/%d_genomes.fasta'%num, 'w') as out:
        out.write(curseq)