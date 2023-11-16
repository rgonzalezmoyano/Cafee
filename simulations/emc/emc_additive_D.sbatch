#!/bin/bash
#SBATCH --job-name=1D-EMC     	 		# Nombre del trabajo
#SBATCH --ntasks=1				        # Una única tarea
#SBATCH --cpus-per-task=1			    # Cores solicitados
#SBATCH --mem-per-cpu=2048		        # Memoria solicitada MB/core
#SBATCH --output=/home/PI/vespana/aces/additive/emc/D/%j.log	    # Fichero de salida
#SBATCH --error=/home/PI/vespana/aces/additive/emc/D/%j.err		# Fichero de error
#SBATCH --mail-user=vespana@umh.es		# Email donde enviar eventos
#SBATCH --mail-type=BEGIN,END,FAIL	    # Eventos a enviar por email
#SBATCH --time=10-0						# Días de duración: 10 días

export OMP_NUM_THREADS=${SLURM_CPUS_ON_NODE}
 
module load R/4.1.2
cd /home/PI/vespana/aces/additive/emc

Rscript D/emc_add_ccnls_D_050.R
Rscript D/emc_add_ccnls_D_100.R
Rscript D/emc_add_ccnls_D_150.R
