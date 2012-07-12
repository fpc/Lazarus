{
    Pascal laszip tool, capable of compressing/uncompressing
    LIDAR files from standard *.las to *.las.lz / *.laz format.

    copyright (C) 2012 felipemonteiro.carvalho@gmail.com

    This software is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
program laszip;

uses SysUtils, Classes;
{#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lasreader.hpp"
#include "laswriter.hpp"}

procedure ExitProgram(AWait: Boolean);
var
  Str: string;
begin
  if AWait then
  begin
    WriteLn('<press ENTER>');
    Read(Str);
  end;
  Halt(1);
end;

procedure Usage(AWait: Boolean = True);
begin
  WriteLn('usage:');
  WriteLn('laszip -i lidar.las');
  WriteLn('laszip -i lidar.laz');
  WriteLn('laszip -i lidar.las -o lidar_compressed.laz');
  WriteLn('laszip -h');
  ExitProgram(AWait);
end;

//static double taketime()
//  return (double)(clock())/CLOCKS_PER_SEC;

var
  i: Integer;
  dry: Boolean = false;
  verbose: Boolean = false;
  file_name_in, file_name_out: string;
  range: Boolean = true;
  start_time: Double = 0;
  file_in, file_out: TFileStream;
begin
  if ParamCount = 1 then
  begin
    WriteLn('enter input file: ');
    ReadLn(file_name_in);
    WriteLn('enter output file: ');
    ReadLn(file_name_out);
  end;

  i := 1;
  while i < ParamCount do
  begin
    if ParamStr(i) = '-dry' then
    begin
      dry := true;
    end
    else if ParamStr(i) = '-verbose' then
    begin
      verbose := true;
    end
    else if ParamStr(i) = '-range' then
    begin
      range = true;
    end
    (*else if (strcmp(argv[i],"-arithmetic") == 0 || strcmp(argv[i],"-arith") == 0)
    begin
      range = false;
    end*)
    else if ParamStr(i) = '-h' then
    begin
      usage();
    end
    else if ParamStr(i) = '-i' then
    begin
      Inc(i);
      file_name_in := ParamStr(i);
    end
    else if ParamStr(i) = '-o' then
    begin
      Inc(i);
      file_name_out = ParamStr(i);
    end
    else
    begin
      WriteLn(Format('cannot understand argument "%s"', [ParamStr(i)]));
      usage();
    end;

    Inc(i);
  end;

  // open input file

  if file_name_in <> '' then
  begin
    try
      file_in := TFileStream.Create(file_name_in, fmRead);
    except
      WriteLn(Format('ERROR: could not open "%s"', [file_name_in]));
      byebye(ParamCount=1);
    end;
  end
  else
  begin
    WriteLn('ERROR: no input specified');
    usage(ParamCount=1);
  end;

  LASreader* lasreader = new LASreader();

  if (!lasreader->open(file_in))
  {
    fprintf(stderr, "ERROR: could not open lasreader\n");
    byebye(argc==1);
  }

  // maybe only a read pass

  if (dry) then
  begin
(*    LASpoint point_min;
    LASpoint point_max;
    double gps_time_min = 0;
    double gps_time_max = 0;
    short rgb_min[3] = {0,0,0};
    short rgb_max[3] = {0,0,0};

    start_time = taketime();

    if (verbose)
    {
      lasreader->read_point();
      point_min = lasreader->point;
      point_max = lasreader->point;
      if (lasreader->points_have_gps_time)
      {
        gps_time_min = lasreader->gps_time;
        gps_time_max = lasreader->gps_time;
      }
      if (lasreader->points_have_rgb)
      {
        rgb_min[0] = lasreader->rgb[0];
        rgb_min[1] = lasreader->rgb[1];
        rgb_min[2] = lasreader->rgb[2];
        rgb_max[0] = lasreader->rgb[0];
        rgb_max[1] = lasreader->rgb[1];
        rgb_max[2] = lasreader->rgb[2];
      }
      while (lasreader->read_point())
      {
        if (lasreader->point.x < point_min.x) point_min.x = lasreader->point.x;
        else if (lasreader->point.x > point_max.x) point_max.x = lasreader->point.x;
        if (lasreader->point.y < point_min.y) point_min.y = lasreader->point.y;
        else if (lasreader->point.y > point_max.y) point_max.y = lasreader->point.y;
        if (lasreader->point.z < point_min.z) point_min.z = lasreader->point.z;
        else if (lasreader->point.z > point_max.z) point_max.z = lasreader->point.z;
        if (lasreader->point.intensity < point_min.intensity) point_min.intensity = lasreader->point.intensity;
        else if (lasreader->point.intensity > point_max.intensity) point_max.intensity = lasreader->point.intensity;
        if (lasreader->point.edge_of_flight_line < point_min.edge_of_flight_line) point_min.edge_of_flight_line = lasreader->point.edge_of_flight_line;
        else if (lasreader->point.edge_of_flight_line > point_max.edge_of_flight_line) point_max.edge_of_flight_line = lasreader->point.edge_of_flight_line;
        if (lasreader->point.scan_direction_flag < point_min.scan_direction_flag) point_min.scan_direction_flag = lasreader->point.scan_direction_flag;
        else if (lasreader->point.scan_direction_flag > point_max.scan_direction_flag) point_max.scan_direction_flag = lasreader->point.scan_direction_flag;
        if (lasreader->point.number_of_returns_of_given_pulse < point_min.number_of_returns_of_given_pulse) point_min.number_of_returns_of_given_pulse = lasreader->point.number_of_returns_of_given_pulse;
        else if (lasreader->point.number_of_returns_of_given_pulse > point_max.number_of_returns_of_given_pulse) point_max.number_of_returns_of_given_pulse = lasreader->point.number_of_returns_of_given_pulse;
        if (lasreader->point.return_number < point_min.return_number) point_min.return_number = lasreader->point.return_number;
        else if (lasreader->point.return_number > point_max.return_number) point_max.return_number = lasreader->point.return_number;
        if (lasreader->point.classification < point_min.classification) point_min.classification = lasreader->point.classification;
        else if (lasreader->point.classification > point_max.classification) point_max.classification = lasreader->point.classification;
        if (lasreader->point.scan_angle_rank < point_min.scan_angle_rank) point_min.scan_angle_rank = lasreader->point.scan_angle_rank;
        else if (lasreader->point.scan_angle_rank > point_max.scan_angle_rank) point_max.scan_angle_rank = lasreader->point.scan_angle_rank;
        if (lasreader->point.user_data < point_min.user_data) point_min.user_data = lasreader->point.user_data;
        else if (lasreader->point.user_data > point_max.user_data) point_max.user_data = lasreader->point.user_data;
        if (lasreader->point.point_source_ID < point_min.point_source_ID) point_min.point_source_ID = lasreader->point.point_source_ID;
        else if (lasreader->point.point_source_ID > point_max.point_source_ID) point_max.point_source_ID = lasreader->point.point_source_ID;
        if (lasreader->point.point_source_ID < point_min.point_source_ID) point_min.point_source_ID = lasreader->point.point_source_ID;
        else if (lasreader->point.point_source_ID > point_max.point_source_ID) point_max.point_source_ID = lasreader->point.point_source_ID;
        if (lasreader->points_have_gps_time)
        {
          if (lasreader->gps_time < gps_time_min) gps_time_min = lasreader->gps_time;
          else if (lasreader->gps_time > gps_time_max) gps_time_max = lasreader->gps_time;
        }
        if (lasreader->points_have_rgb)
        {
          if (lasreader->rgb[0] < rgb_min[0]) rgb_min[0] = lasreader->rgb[0];
          else if (lasreader->rgb[0] > rgb_max[0]) rgb_max[0] = lasreader->rgb[0];
          if (lasreader->rgb[1] < rgb_min[1]) rgb_min[1] = lasreader->rgb[1];
          else if (lasreader->rgb[1] > rgb_max[1]) rgb_max[1] = lasreader->rgb[1];
          if (lasreader->rgb[2] < rgb_min[2]) rgb_min[2] = lasreader->rgb[2];
          else if (lasreader->rgb[2] > rgb_max[2]) rgb_max[2] = lasreader->rgb[2];
        }
      }
      fprintf(stderr, "x %d %d %d\n",point_min.x, point_max.x, point_max.x - point_min.x);
      fprintf(stderr, "y %d %d %d\n",point_min.y, point_max.y, point_max.y - point_min.y);
      fprintf(stderr, "z %d %d %d\n",point_min.z, point_max.z, point_max.z - point_min.z);
      fprintf(stderr, "intensity %d %d %d\n",point_min.intensity, point_max.intensity, point_max.intensity - point_min.intensity);
      fprintf(stderr, "edge_of_flight_line %d %d %d\n",point_min.edge_of_flight_line, point_max.edge_of_flight_line, point_max.edge_of_flight_line - point_min.edge_of_flight_line);
      fprintf(stderr, "scan_direction_flag %d %d %d\n",point_min.scan_direction_flag, point_max.scan_direction_flag, point_max.scan_direction_flag - point_min.scan_direction_flag);
      fprintf(stderr, "number_of_returns_of_given_pulse %d %d %d\n",point_min.number_of_returns_of_given_pulse, point_max.number_of_returns_of_given_pulse, point_max.number_of_returns_of_given_pulse - point_min.number_of_returns_of_given_pulse);
      fprintf(stderr, "return_number %d %d %d\n",point_min.return_number, point_max.return_number, point_max.return_number - point_min.return_number);
      fprintf(stderr, "classification %d %d %d\n",point_min.classification, point_max.classification, point_max.classification - point_min.classification);
      fprintf(stderr, "scan_angle_rank %d %d %d\n",point_min.scan_angle_rank, point_max.scan_angle_rank, point_max.scan_angle_rank - point_min.scan_angle_rank);
      fprintf(stderr, "user_data %d %d %d\n",point_min.user_data, point_max.user_data, point_max.user_data - point_min.user_data);
      fprintf(stderr, "point_source_ID %d %d %d\n",point_min.point_source_ID, point_max.point_source_ID, point_max.point_source_ID - point_min.point_source_ID);
      if (lasreader->points_have_gps_time)
      {
        fprintf(stderr, "gps_time %.8f %.8f %.8f\n",gps_time_min, gps_time_max, gps_time_max - gps_time_min);
      }
      if (lasreader->points_have_rgb)
      {
        fprintf(stderr, "R %d %d %d\n",rgb_min[0], rgb_max[0], rgb_max[0] - rgb_min[0]);
        fprintf(stderr, "G %d %d %d\n",rgb_min[1], rgb_max[1], rgb_max[1] - rgb_min[1]);
        fprintf(stderr, "B %d %d %d\n",rgb_min[2], rgb_max[2], rgb_max[2] - rgb_min[2]);
      }
    }
    else
    {
      while (lasreader->read_point());
    }
    fprintf(stderr,"total reading time: %g seconds\n", taketime()-start_time);*)
  end
  else
  begin
    // create output file name if needed

    if (file_name_out == 0 && !olas && !olaz)
    {
      int len = strlen(file_name_in);
      file_name_out = strdup(file_name_in);
      if (strstr(file_name_in, ".las"))
      {
        file_name_out[len-4] = '.';
        file_name_out[len-3] = 'l';
        file_name_out[len-2] = 'a';
        file_name_out[len-1] = 'z';
      }
      else if (strstr(file_name_in, ".laz"))
      {
        file_name_out[len-4] = '.';
        file_name_out[len-3] = 'l';
        file_name_out[len-2] = 'a';
        file_name_out[len-1] = 's';
      }
      else if (strstr(file_name_in, ".las.lz"))
      {
        file_name_out[len-7] = '.';
        file_name_out[len-6] = 'l';
        file_name_out[len-5] = 'a';
        file_name_out[len-4] = 's';
        file_name_out[len-3] = '\0';
      }
      else if (strstr(file_name_in, ".LAS"))
      {
        file_name_out[len-4] = '.';
        file_name_out[len-3] = 'L';
        file_name_out[len-2] = 'A';
        file_name_out[len-1] = 'Z';
      }
      else if (strstr(file_name_in, ".LAZ"))
      {
        file_name_out[len-4] = '.';
        file_name_out[len-3] = 'L';
        file_name_out[len-2] = 'A';
        file_name_out[len-1] = 'S';
      }
      else if (strstr(file_name_in, ".LAS.LZ"))
      {
        file_name_out[len-7] = '.';
        file_name_out[len-6] = 'L';
        file_name_out[len-5] = 'A';
        file_name_out[len-4] = 'S';
        file_name_out[len-3] = '\0';
      }
    }

    // open output file

    int compression = 0;

    FILE* file_out = 0;
    if (file_name_out)
    {
      file_out = fopen(file_name_out, "wb");
      if (strstr(file_name_out, ".laz") || strstr(file_name_out, ".las.lz") || strstr(file_name_out, ".LAZ") || strstr(file_name_out, ".LAS.LZ"))
      {
        compression = 1;
      }
    }
    else if (olas || olaz)
    {
      file_out = stdout;
      if (olaz)
      {
        compression = 1;
      }
    }
    else
    {
      fprintf (stderr, "ERROR: no output specified\n");
      usage(argc==1);
    }

    if (verbose) start_time = taketime();

    LASwriter* laswriter = new LASwriter();

    if (compression)
    {
      if (range)
      {
        compression = LAS_COMPRESSION_RANGE;
      }
      else
      {
        compression = LAS_COMPRESSION_ARITHMETIC;
      }
    }

    if (laswriter->open(file_out, &(lasreader->header), compression) == false)
    {
      fprintf(stderr, "ERROR: could not open laswriter\n");
      byebye(argc==1);
    }

    // loop over points

    while (lasreader->read_point())
    {
      if (lasreader->p_count == 40)
      {
        lasreader->p_count = 40;
      }
      laswriter->write_point(&(lasreader->point), lasreader->gps_time, lasreader->rgb);
    }

    int total_bytes = laswriter->close();

    if (verbose) fprintf(stderr,"total compression: %g sec %d bytes for %d points of format %d\n", taketime()-start_time, total_bytes, lasreader->p_count, lasreader->header.point_data_format);

    delete laswriter;
    fclose(file_out);
  }

  lasreader.close();
  lasreader.Free;
  file_in.Free;

  ExitProgram(argc==1);

  //return 0;
end.

