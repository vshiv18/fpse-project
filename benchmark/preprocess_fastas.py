import argparse
import os
from random import shuffle

from Bio import SeqIO
from tqdm import tqdm

workdir = os.path.dirname(os.path.realpath(__file__))

parser = argparse.ArgumentParser()
parser.add_argument("--raw-dir", type=str, default=os.path.join(workdir, "raw_fastas"))
group = parser.add_mutually_exclusive_group(required=True)
group.add_argument("--all", action="store_true", default=False)
group.add_argument(
    "--steps", nargs="+", default=[1, 5, 25, 125, 625, 3125, 15625], type=int
)

args = parser.parse_args()
raw_dir = args.raw_dir
all_steps = args.all
steps = args.steps

outdir = os.path.join(workdir, "cum_fastas")
os.makedirs(outdir, exist_ok=True)

raw_files = os.listdir(raw_dir)
raw_files = list(filter(lambda x: x.endswith(".fasta"), raw_files))

seqs = {}
for filename in tqdm(raw_files):
    for seq in SeqIO.parse(os.path.join(raw_dir, filename), "fasta"):
        seq_id = seq.id
        if seq_id not in seq:
            seqs[seq_id] = "".join(
                filter(lambda x: x.upper() in ["A", "T", "C", "G"], seq.seq)
            )

seq_list = list(seqs.values())

steps = range(1, len(seq_list) + 1) if all_steps else steps
for num in steps:
    shuffle(seq_list)
    curseq = "".join(seq_list[:num])
    with open(os.path.join(outdir, f"{num}_genomes.fasta"), "w") as out:
        out.write(curseq)
