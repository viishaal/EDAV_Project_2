#!/usr/bin/env python
# coding=utf-8

import sys
import pandas as pd

def cleanMainCause(input):
        input = str(input)
        if input in ['', '0', 'nan', 'see notes']:
                return 'Others'
        elif any(s in input for s in ['Heavy Rain', 'HeavyRain', 'Heay Rain', 'Heavy  Rain', 'Torrential', 'Rainy', 'torrential', 'Rain', 'heavy rain', 'Heavy seasonal rains', 'Heavy rain']):
                return 'Heavy Rain'
        elif any(s in input for s in ['Dam', 'Levy', 'Levee']):
                return 'Dam Malfunction'
        elif any(s in input for s in ['Monsoonal', 'Monsoon', 'monsoon', 'Monoonal']):
                return 'Monsoonal Rain'
        elif any(s in input for s in ['Tropical', 'tropical', 'Typhoon', 'Hurricane']):
                return 'Tropical Cyclone'
        elif any(s in input for s in ['Snow', 'snowmelt']):
                return 'Snow'
        elif any(s in input for s in ['Storm']):
                return 'Storm'
        elif any(s in input for s in ['Tide', 'Tidal']):
                return 'Tide Surge'
        elif any(s in input for s in ['Ice', 'J']):
                return 'Ice Movement'
        elif any( s in input for s in ['Avalance', 'Avalanche']):
                return 'Avalance'
        else:
                return input
if __name__ == '__main__':
        try:
                filename = sys.argv[1]
        except:
                filename = 'cleaned_flood.csv'
        raw = pd.read_csv(filename)
        raw[u'Main cause'] = raw[u'Main cause'].apply(cleanMainCause, 1)
        print raw[u'Main cause'].unique()
        raw.to_csv('Cleaned_main_cause.csv')
