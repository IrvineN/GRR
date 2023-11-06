import sys
import csv
import os

# Global Variable from R script
in_name = sys.argv[1]
partID = sys.argv[2]
out_name = sys.argv[3]
data_path = sys.argv[4]
site_id = sys.argv[5]

os.chdir(data_path)   
with open(in_name) as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    rows=[]
    part_ids_new=[]
    site_ids_new=[] 
    new_partnum = str(partID).strip()  #remove carriage returns if using batch file
    new_site_id = str(site_id).strip()
    #print('Replacing original part numbers in '+input_name_matrix[run_csv][site_csv]+' with: '+new_partnum+' and writing out to '+output_name_matrix[run_csv][site_csv])
            
    for row in csv_reader:
        rows.append(row)
                
    partid_row = rows[1]    # partid is usually on the second row of the csv file
    siteid_row = rows[9]     
    part_id_loc = partid_row.index("part_id")
    site_id_loc = siteid_row.index("site")
            
# slice row into two parts to pull out original part numbers   
    partid_index = partid_row[:part_id_loc+1]    
    part_ids = partid_row[part_id_loc+1:]

    siteid_index = siteid_row[:site_id_loc+1]
    site_ids = siteid_row[site_id_loc+1:]

# interate across all part numbers in file and replace with new part number
    for num in part_ids:
        part_ids_new.append(new_partnum)
        site_ids_new.append(new_site_id)

# merge part numbers back into row     
    partid_index.extend(part_ids_new)
    rows[1] = partid_index
        
    siteid_index.extend(site_ids_new)
    rows[9] = siteid_index
        
#write to a new csv file
with open(out_name,'w', newline='') as csvfile:
    csvwriter = csv.writer(csvfile)
    csvwriter.writerows(rows)
        
print('Csv Update Finished')