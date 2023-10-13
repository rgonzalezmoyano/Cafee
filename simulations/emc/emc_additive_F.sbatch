#!/bin/bash
#SBATCH --job-name=1F-EMC     	 		# Nombre del trabajo
#SBATCH --ntasks=1				        # Una única tarea
#SBATCH --cpus-per-task=1			    # Cores solicitados
#SBATCH --mem-per-cpu=2048		        # Memoria solicitada MB/core
#SBATCH --output=/home/PI/vespana/aces/additive/emc/F/%j.log	    # Fichero de salida
#SBATCH --error=/home/PI/vespana/aces/additive/emc/F/%j.err		# Fichero de error
#SBATCH --mail-user=vespana@umh.es		# Email donde enviar eventos
#SBATCH --mail-type=BEGIN,END,FAIL	    # Eventos a enviar por email
#SBATCH --time=10-0						# Días de duración: 10 días

export OMP_NUM_THREADS=${SLURM_CPUS_ON_NODE}
 
module load R/4.1.2
cd /home/PI/vespana/aces/additive/emc

Rscript F/emc_add_ccnls_F_050.R
Rscript F/emc_add_ccnls_F_100.R
Rscript F/emc_add_ccnls_F_150.R
