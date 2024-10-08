#!/bin/bash

# Assign command-line arguments to variables
inds=$1.inds
chr=$2
midchrom=$3
gen1=$4
gen2=$5

# Actual VCF production begins here

# Extract genotype information from VCF file and save it to ${1}.gt
zcat ${1}.vcf.gz | awk 'f;/CHROM/{f=1}' - | cut -f10- > ${1}.gt

# Process VCF file to create a modified VCF with updated chromosome and position information
zcat ${1}.vcf.gz | awk 'f;/CHROM/{f=1}' - | cut -f2 | awk -v chr="$chr" -v midchrom="$midchrom" '{ if ($1 >= midchrom) print "chr"chr+1"\t"$1-midchrom"\t.\tA\tG\t.\tPASS\tAA=A\tGT"; else print "chr"chr"\t"$1"\t.\tC\tT\t.\tPASS\tAA=C\tGT"}' | paste - ${1}.gt > tmp${1}

# Sort and process index file for VCF creation
sort -k1n $1.inds | cut -f1 | tr "\n" "\t" | awk '{print "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\t"$0}' | awk '{ sub(/[ \t]+$/, ""); print }' | cat - tmp${1} > tmp${1}.vcf

# Create a temporary header file and concatenate with the modified VCF
zcat ${1}.vcf.gz | head -n5  > tmp_${1}h.vcf
zcat ${1}.vcf.gz | head -n4 | tail -1 | sed 's/chr21/chr22/g' >> tmp_${1}h.vcf
echo '##INFO=<ID=AA,Number=2,Type=Flag,Description="AA">' >> tmp_${1}h.vcf
cat tmp_${1}h.vcf tmp${1}.vcf > tmp_${1}.vcf

# Clean up temporary files
rm tmp_${1}h.vcf
rm tmp${1}
mv tmp_${1}.vcf ${1}_tmp.vcf
rm tmp${1}.vcf

# Wait for background processes to finish
wait

# Normalize and compress the modified VCF
bcftools norm -d all -Oz -o ${1}.vcf.gz ${1}_tmp.vcf

# Index the final VCF file
bcftools index -f ${1}.vcf.gz

wait

# Clean up temporary files
rm ${1}_tmp.vcf


## if ${1} contains "_gwas" then proceed
# This section for GWAS
# Creates phenotype file for samples
## Else if iHS then just remove .pos, .inds, .gt

if [[ "$1" == *"gwas"* ]]; then
	 mv ${1}.vcf.gz ${1}_${4}_${5}.vcf.gz
	 bcftools index -f ${1}_${4}_${5}.vcf.gz
	 sort -k1n ${1}.inds | awk '{ if ($2 == "Dead") {print $1"\t"$1"\t"$2"\t1"}  else {print $1"\t"$1"\t"$2"\t2"} }' > ${1}_${4}_${5}.pheno
	 rm ${1}.gt
	 rm ${1}.inds

    plink --vcf ${1}_${4}_${5}.vcf.gz \
    --allow-no-sex \
    --geno 0.001 \
    --maf 0.05 \
    --logistic \
    --adjust \
    --seed 62 \
    --pheno ${1}_${4}_${5}.pheno \
    --mpheno 2 \
    --set-missing-var-ids @_# \
    --out ${1}_add

    grep -v NA ${1}_add.assoc.logistic | awk 'NR==FNR{c[$2]++;next};c[$2]' - ${1}_add.assoc.logistic.adjusted | sort -n -t_ -k1,1 -k2,2 > ${1}_add.tmp
    grep -v NA ${1}_add.assoc.logistic | paste - ${1}_add.tmp | tr -s ' ' '\t' | cut -f1,11,12,13 --complement | gzip > ${1}_${4}_${5}_add.assoc.out.gz
    rm ${1}_add.tmp
    rm ${1}_add.assoc.logistic
    rm ${1}_add.assoc.logistic.adjusted


    plink --vcf ${1}_${4}_${5}.vcf.gz \
    --allow-no-sex \
    --geno 0.001 \
    --maf 0.05 \
    --logistic recessive \
    --model rec \
    --adjust \
    --seed 62 \
    --pheno ${1}_${4}_${5}.pheno \
    --mpheno 2 \
    --set-missing-var-ids @_# \
    --out ${1}_rec

    rm *.nosex

    grep -v NA ${1}_rec.assoc.logistic | awk 'NR==FNR{c[$2]++;next};c[$2]' - ${1}_rec.assoc.logistic.adjusted | sort -n -t_ -k1,1 -k2,2 > ${1}_rec.tmp
    grep -v NA ${1}_rec.assoc.logistic | paste - ${1}_rec.tmp | tr -s ' ' '\t' | cut -f1,11,12,13 --complement | gzip > ${1}_${4}_${5}_rec.assoc.out.gz
    rm ${1}_rec.tmp
    rm ${1}_rec.assoc.logistic
    rm ${1}_rec.assoc.logistic.adjusted
    gzip --force ${1}_rec.model

elif [[ $gen1 == $gen2 && -f "${1}_chr${chr}.vcf.gz" ]]; then
	 bcftools index -f ${1}.vcf.gz
	 mv ${1}_chr${chr}.vcf.gz ${1}_chr${chr}_d.vcf.gz
	 bcftools view -r chr${chr} -Oz -o ${1}_chr${chr}_a.vcf.gz ${1}.vcf.gz
	 chr=$(echo $chr+1 | bc -l)
	 mv ${1}_chr${chr}.vcf.gz ${1}_chr${chr}_d.vcf.gz
	 bcftools view -r chr${chr} -Oz -o ${1}_chr${chr}_a.vcf.gz ${1}.vcf.gz
	 rm ${1}.vcf.gz*
	 rm ${1}.gt
	 rm $inds

else
	 bcftools index -f ${1}.vcf.gz
	 bcftools view -r chr${chr} -Oz -o ${1}_chr${chr}.vcf.gz ${1}.vcf.gz
	 bcftools index -f ${1}_chr${chr}.vcf.gz	 
	 chr=$(echo $chr+1 | bc -l)
	 bcftools view -r chr${chr} -Oz -o ${1}_chr${chr}.vcf.gz ${1}.vcf.gz
	 bcftools index -f ${1}_chr${chr}.vcf.gz
	 rm ${1}.vcf.gz*
	 rm ${1}.gt
	 rm $inds
fi
wait
