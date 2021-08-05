#! /bin/bash
#SBATCH --job-name=algo2cust500
#SBATCH --output=trace_%a.out
#SBATCH --error=error_%a.err
#SBATCH --mem-per-cpu=4096
#SBATCH --time=72:00:00
#SBATCH --array=1-500
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --export=NONE
module load R/4.0.5
Rscript job-algo2-500cust.R $SLURM_ARRAY_TASK_ID 

