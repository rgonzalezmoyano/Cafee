#!/bin/bash
#SBATCH --job-name=1A-EMC     	 		# Nombre del trabajo
#SBATCH --ntasks=1				        # Una única tarea
#SBATCH --cpus-per-task=1			    # Cores solicitados
#SBATCH --mem-per-cpu=1024		        # Memoria solicitada MB/core
#SBATCH --output=/home/PI/vespana/aces/additive/emc/A/%j.log	    # Fichero de salida
#SBATCH --error=/home/PI/vespana/aces/additive/emc/A/%j.err		# Fichero de error
#SBATCH --mail-user=vespana@umh.es		# Email donde enviar eventos
#SBATCH --mail-type=BEGIN,END,FAIL	    # Eventos a enviar por email
#SBATCH --time=3-0						# Días de duración: 3 días

export OMP_NUM_THREADS=${SLURM_CPUS_ON_NODE}
 
module load R/4.1.2
cd /home/PI/vespana/aces/additive/emc

Rscript A/emc_add_ccnls_A_050.R
Rscript A/emc_add_ccnls_A_100.R
Rscript A/emc_add_ccnls_A_150.R
