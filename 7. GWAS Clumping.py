%%bash
``
for i in {1..22};
do
    echo "-----------------> Starting with Chr $i <-------------------"

    #Download the exome file from the bucket.
    gsutil -m cp -r \
    gs://fc-secure-777a9168-f04a-464b-9b69-00e5a517eeff/notebooks/data/ACAF_E/extractedChr$i.EuropeanCohort.PostQC.* \
       /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/data/
    
    plink2 --bfile /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/data/extractedChr$i.EuropeanCohort.PostQC \
        --clump /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/European/asthma_e_uw_$i.pvalues \
        --clump-p1 1e-4 \
        --clump-p2 1e-2 \
        --clump-r2 0.1 \
        --clump-kb 3000 \
        --clump-snp-field Predictor \
        --chr $i \
        --out /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/clumping/asthma_e_uw_clump_$i \
        --threads 8 \
        --memory 55000
    
    plink2 --bfile /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/data/extractedChr$i.EuropeanCohort.PostQC \
        --clump /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/European/asthma_e_w_$i.pvalues \
        --clump-p1 1e-4 \
        --clump-p2 1e-2 \
        --clump-r2 0.1 \
        --clump-kb 3000 \
        --clump-snp-field Predictor \
        --chr $i \
        --out /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/clumping/asthma_e_w_clump_$i \
        --threads 8 \
        --memory 55000

    #Remove the exome file.
    rm /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/data/extractedChr$i.EuropeanCohort.PostQC.*
    
    echo "-----------------> Finished with Chr $i <-------------------"
    
done

head -n 1 /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/clumping/asthma_e_uw_clump_1.clumps > /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/clumping/combined_asthma_e_uw_results.clumps
tail -n +2 -q /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/clumping/asthma_e_uw_clump_*.clumps >> /home/jupyter/workspaces/allofusweightingstudynew/wGWAS/output/clumping/combined_asthma_e_uw_results.clumps
