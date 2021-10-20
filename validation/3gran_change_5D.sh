#! /bin/bash
#SBATCH --job-name=3gran_change_5D
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=8G
#SBATCH --time=80:00:00
#SBATCH --array=1-54
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/4.0.5
Rscript gran3_change_5D.R $SLURM_ARRAY_TASK_ID 

