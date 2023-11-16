#!/bin/bash
#SBATCH --job-name=1E-EMC     	 		# Nombre del trabajo
#SBATCH --ntasks=1				        # Una única tarea
#SBATCH --cpus-per-task=1			    # Cores solicitados
#SBATCH --mem-per-cpu=1024		        # Memoria solicitada MB/core
#SBATCH --output=/home/PI/vespana/aces/additive/emc/E/%j.log	    # Fichero de salida
#SBATCH --error=/home/PI/vespana/aces/additive/emc/E/%j.err		# Fichero de error
#SBATCH --mail-user=vespana@umh.es		# Email donde enviar eventos
#SBATCH --mail-type=BEGIN,END,FAIL	    # Eventos a enviar por email
#SBATCH --time=6-0						# Días de duración: 6 días

export OMP_NUM_THREADS=${SLURM_CPUS_ON_NODE}
 
module load R/4.1.2
cd /home/PI/vespana/aces/additive/emc

Rscript E/emc_add_ccnls_E_050.R
Rscript E/emc_add_ccnls_E_100.R
Rscript E/emc_add_ccnls_E_150.R
