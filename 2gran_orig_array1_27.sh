#! /bin/bash
#SBATCH --job-name=gran2_orig
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=32G
#SBATCH --time=80:00:00
#SBATCH --array=1-27
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/4.0.5
Rscript gran2_orig.R $SLURM_ARRAY_TASK_ID 

