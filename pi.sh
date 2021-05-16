#!/bin/bash
#SBATCH --qos=debug
#SBATCH --time=00:30:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=32
#SBATCH --constraint=haswell     
#SBATCH --output="pi.out"

# Unload in case any of them is preloaded
module unload PrgEnv-cray
module unload PrgEnv-gnu
module unload PrgEnv-intel
# Load necessary modules
module load PrgEnv-cray

cd $SLURM_SUBMIT_DIR

# executable
EXE=PI
# compile
ftn -o ${EXE} monte_carlo_pi.f90

TIME_START=$(date -u +%s)
srun ./${EXE}
TIME_END=$(date -u +%s)

# Calculate time span
declare -i SEC_IN_MIN=60
declare -i SEC_IN_HOUR=3600
HOUR_VAL=$(((TIME_END - TIME_START) / SEC_IN_HOUR))
MIN_VAL=$(((TIME_END - TIME_START) / SEC_IN_MIN))
SEC_VAL=$(((TIME_END - TIME_START) % SEC_IN_MIN))
echo "Time spent: $HOUR_VAL hour $MIN_VAL min $SEC_VAL sec"
