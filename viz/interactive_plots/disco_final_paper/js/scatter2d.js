 var CSV_PREFIX = "data/";
 var CSV_URL = "disco_bpca_scores_20181229_rev_highlight.csv";
 var REFERENCE_URL = "reference_points.csv";

 var DATASET_PREFIX = "data/dataset/";
 var DATASET_URLS = ["NHSDiscography_Annotate.csv", "NHSDiscography_TranscriptionFeatures.csv", "NHSDiscography_Metadata.csv"];

 var disco = {

     index: "indx",
     x: "bpca_1",
     y: "bpca_2",
     
     xAlias : "PC1: Melodic Complexity",
     yAlias : "PC2: Rhythmic Complexity",
     
     audio_prefix: "audio/",
     
     title : "NHS Discography Explorer",

     indices: {

         NHSDiscography_Annotate: "song", //index
         NHSDiscography_TranscriptionFeatures: "song", //index
         NHSDiscography_Metadata: "song" //song

     },

     color: [

         {
             index: "na", //0-30 
             option: "NA",
             dict: [{
                 l: "na"
             }],
             colors: ["#808080"],
             f: function (n_) {
                 return 0;
             }

        },

         {
             index: "NHSDiscography_Metadata.type", //song_type
             option: "Song Type",
             dict: [{
                     l: "Dance"
                }, {
                     l: "Healing"
                }, {
                     l: "Love"
                },
                 {
                     l: "Lullaby"
                }
            ],	         
	     colors: ["#6D98F3", "#E96465", "#F9C659", "#6AB878"],
             format: null,
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     var isEquel = JSON.stringify(n_) === JSON.stringify(d_.l);
                     if (isEquel) {
                         found = i_;
                     }

                 });

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.range", //0-30 
             option: "Pitch Range",

             dict: [{
                     l: "0.0-5.0",
                     d: {
                         s: 0.0,
                         e: 5.0
                     }
                 },
                 {
                     l: "5.0-10.0",
                     d: {
                         s: 5.0,
                         e: 10.0
                     }
                 },
                 {
                     l: "10.0-15.0",
                     d: {
                         s: 10.0,
                         e: 15.0
                     }
                 },
                 {
                     l: "15.0-20.0",
                     d: {
                         s: 15.0,
                         e: 20.0
                     }
                 },
                 {
                     l: "20.0-25.0",
                     d: {
                         s: 20.0,
                         e: 25.0
                     }
                 },
                 {
                     l: ">25.0",
                     d: {
                         s: 25.0,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],

             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_) {

                 var found = 0;

                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.mean_interval", //float
             option: "Mean Interval Size",
             dict: [{
                     l: "0.0-1.0",
                     d: {
                         s: 0.0,
                         e: 1.0
                     }
                 },
                 {
                     l: "1.0-2.0",
                     d: {
                         s: 1.0,
                         e: 2.0
                     }
                 },
                 {
                     l: "2.0-3.0",
                     d: {
                         s: 2.0,
                         e: 3.0
                     }
                 },
                 {
                     l: "3.0-4.0",
                     d: {
                         s: 3.0,
                         e: 4.0
                     }
                 },
                 {
                     l: "4.0-5.0",
                     d: {
                         s: 4.0,
                         e: 5.0
                     }
                 },
                 {
                     l: ">5.0",
                     d: {
                         s: 5.0,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],

             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.rel_strength_modal_intervals", //float
             option: "Strength of Modal Intervals",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],

             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.common_intervals_count", //float
             option: "Number of Common Intervals",
             dict: [{
                     l: "0.0-1.0",
                     d: {
                         s: 0.0,
                         e: 1.0
                     }
                 },
                 {
                     l: "1.0-2.0",
                     d: {
                         s: 1.0,
                         e: 2.0
                     }
                 },
                 {
                     l: "2.0-3.0",
                     d: {
                         s: 2.0,
                         e: 3.0
                     }
                 },
                 {
                     l: ">4.0",
                     d: {
                         s: 4.0,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.rel_strength_top_pitchcls", //float
             option: "Strength of Modal Pitch Class",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.melodic_thirds", //float
             option: "Prevalence of Melodic Thirds",
             dict: [{
                     l: "0.00-0.10",
                     d: {
                         s: 0.0,
                         e: 0.10
                     }
                 },
                 {
                     l: "0.10-0.20",
                     d: {
                         s: 0.10,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.30",
                     d: {
                         s: 0.20,
                         e: 0.30
                     }
                 },
                 {
                     l: "0.30-0.40",
                     d: {
                         s: 0.30,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.50",
                     d: {
                         s: 0.40,
                         e: 0.50
                     }
                 },
                 {
                     l: ">0.50",
                     d: {
                         s: 0.50,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.modal_interval_prevalence", //float
             option: "Prevalence of Modal Interval",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.00,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.stepwise_motion", //float
             option: "Prevalence of Stepwise Motion",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.8-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.dynamics", //float
             option: "Dymamics",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.ornament", //binary
             option: "Ornamentation",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.tension", //binary
             option: "Tension and Release",
             dict: [{
                     l: "0.0-1.0",
                     d: {
                         s: 0.0,
                         e: 1.0
                     }
                 },
                 {
                     l: "1.0-2.0",
                     d: {
                         s: 1.0,
                         e: 2.0
                     }
                 },
                 {
                     l: ">2.0",
                     d: {
                         s: 2.0,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.variation_melodic",
             option: "Melodic Variation",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             colors: d3.scaleSequential(d3["interpolateCool"]).domain([0, 6]),
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {
                         found = i_;
                     }


                 })

                 return found;

             }
        },


    ],

     radius: [

         {
             index: "na",
             option: "NA",
             dict: [{ l: "na", r: null }],
             format: null,
             f: null
        },
         {
             index: "NHSDiscography_Annotate.accent", // 0, 0.5, 1.0
             option: "Accent",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.macrometer_ord", //1 - 6
             option: "Strength of Macrometer",
             dict: [{
                     l: "<2.0",
                     d: {
                         s: 0,
                         e: 2.0
                     }
                 },
                 {
                     l: "2.0-3.0",
                     d: {
                         s: 2.0,
                         e: 3.0
                     }
                 },
                 {
                     l: "3.0-4.0",
                     d: {
                         s: 3.0,
                         e: 4.0
                     }
                 },
                 {
                     l: "4.0-5.0",
                     d: {
                         s: 4.0,
                         e: 5.0
                     }
                 },
                 {
                     l: ">5.0",
                     d: {
                         s: 5.0,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],
             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.ritard_accel", // 0, 1
             option: "Changes in Tempo",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.tempo_adj", //60 - 204
             option: "Tempo",
             dict: [{
                     l: "<100",
                     d: {
                         s: 0,
                         e: 100
                     }
                 },
                 {
                     l: "100-120",
                     d: {
                         s: 100,
                         e: 120
                     }
                 },
                 {
                     l: "120-140",
                     d: {
                         s: 120,
                         e: 140
                     }
                 },
                 {
                     l: "140-160",
                     d: {
                         s: 140,
                         e: 160
                     }
                 },
                 {
                     l: "160-180",
                     d: {
                         s: 160,
                         e: 180
                     }
                 },
                 {
                     l: "180-200",
                     d: {
                         s: 180,
                         e: 200
                     }
                 },
                 {
                     l: ">200",
                     d: {
                         s: 200,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],
             format: {
                 d: 1,
                 f: 0
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_Annotate.variation_rhythmic",
             option: "Rythmic Variation",
             dict: [{
                     l: "0.00-0.20",
                     d: {
                         s: 0.0,
                         e: 0.20
                     }
                 },
                 {
                     l: "0.20-0.40",
                     d: {
                         s: 0.20,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.60",
                     d: {
                         s: 0.40,
                         e: 0.60
                     }
                 },
                 {
                     l: "0.60-0.80",
                     d: {
                         s: 0.60,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.00",
                     d: {
                         s: 0.80,
                         e: 1.00
                     }
                 }],
             format: {
                 d: 0.01,
                 f: 2
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 return found;

             }
        },

         {
             index: "NHSDiscography_TranscriptionFeatures.average_note_duration", //float 0 - 2.1
             option: "Average Note Duration",
             dict: [{
                     l: "0.00-0.40",
                     d: {
                         s: 0.0,
                         e: 0.40
                     }
                 },
                 {
                     l: "0.40-0.80",
                     d: {
                         s: 0.40,
                         e: 0.80
                     }
                 },
                 {
                     l: "0.80-1.20",
                     d: {
                         s: 0.80,
                         e: 1.20
                     }
                 },
                 {
                     l: "1.20-1.60",
                     d: {
                         s: 1.20,
                         e: 1.60
                     }
                 },
                 {
                     l: ">1.60",
                     d: {
                         s: 1.60,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],
             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 return found;

             }

        },

         {
             index: "NHSDiscography_TranscriptionFeatures.note_density", // float 0.0 - 7.0
             option: "Note Density",
             dict: [{
                     l: "0.00-1.00",
                     d: {
                         s: 0.0,
                         e: 1.00
                     }
                 },
                 {
                     l: "1.00-2.00",
                     d: {
                         s: 1.00,
                         e: 2.00
                     }
                 },
                 {
                     l: "2.00-3.00",
                     d: {
                         s: 2.00,
                         e: 3.00
                     }
                 },
                 {
                     l: "3.00-4.00",
                     d: {
                         s: 3.00,
                         e: 4.00
                     }
                 },
                 {
                     l: "4.00-5.00",
                     d: {
                         s: 4.00,
                         e: 5.00
                     }
                 },
                 {
                     l: "5.00-6.00",
                     d: {
                         s: 5.00,
                         e: 6.00
                     }
                 },
                 {
                     l: ">6.01",
                     d: {
                         s: 6.01,
                         e: Number.POSITIVE_INFINITY
                     }
                 }],

             format: {
                 d: 0.1,
                 f: 1
             },
             f: function (n_, key_) {

                 var found = 0;
                 this.dict.forEach(function (d_, i_) {

                     if (Number(n_).between([d_.d.s, d_.d.e])) {

                         if (Number(n_) > 6) {
                             console.log(n_);
                             console.log(Number(n_).between([6.01, Number.POSITIVE_INFINITY]));
                         }
                         if (key_ == "i") {
                             found = i_;
                         }
                         if (key_ == "r") {
                             found = d_.r;
                         }

                     }

                 })

                 if (Number(n_) > 6 && key_ == "i") {
                     console.log("WTF??:" + found);
                 }
                 return found;

             }
        },

    ],

     popup: [

         {
             index: "NHSDiscography_Metadata.type"
        },
         {
             index: "NHSDiscography_Metadata.culture"
        },

         {
             index: "NHSDiscography_Metadata.hraf_region"
        },

         {
             index: "NHSDiscography_Metadata.hraf_subregion"
        },
         {
             index: "NHSDiscography_Metadata.year"
        },
         {
             index: "NHSDiscography_Metadata.collector_name"
        },
         {
             index: "NHSDiscography_Metadata.citation"
        },
         {
             index: "NHSDiscography_Metadata.permalink"
        }
    ],

     checkbox: {

         legend: ["Show/Hide"],
         dictionary: {

             "EmberÃ¡": "Emberá",
             "JavaÃ©": "Javaé"

         },

         cultures: [],
         regions: [],
         index: [],
         dict : {
             
             type : "Song Type",
             culture : "Society",
             hraf_region : "World Region",
             hraf_subregion : "World Subregion",
             year : "Year Collected",
             collector_name : "Collector Name",
             citation : "Citation",
             permalink : "URL"
             
         }

     }

 };

 var parameters = {

     references: [],
     referenceDimensions: {width: 16, height: 16},
     referenceColorsFromData: false,
     referenceDarkerRatio: 1.0,
     referenceTrigger: "mouseover",
     popup: ["audio", "year", "collector_name", "citation", "permalink", "hraf_region", "hraf_subregion", "culture"],

     defaultAlpha : 0.5,         //for all points except highlighted & referenced
     
     highlightedAlpha : 1.0,
     highlightedDarker : 0.7,

     zoomLimits : [0.125, 8], //first zooming OUT, second zoom IN
     color: disco.color[1],
     na_color: "#808080",
     radius: disco.radius[1],
     radiusScale: d3.scaleLinear().domain([1, 10]).range([4.9, 20]),
     margin: {
         t: 64,
         r: 64,
         b: 80,
         l: 304
     }
 };

disco.radius[0].f = function (n_, key_) { if(key_ == "i") { return 0; } else { return parameters.radiusScale(1); }}
disco.radius[0].dict[0].r = parameters.radiusScale(1);

 var limits = {

     x: {
         min: Number.POSITIVE_INFINITY,
         max: Number.NEGATIVE_INFINITY
     },
     y: {
         min: Number.POSITIVE_INFINITY,
         max: Number.NEGATIVE_INFINITY
     }
 };

 var svg, div, legendBox, legendParameters, w = window.innerWidth,
     h = window.innerHeight;
 var gX, gY, tX, tY, gridX, gridY;
 var points = [];
 var db = [];

 var zoom = d3.zoom().scaleExtent(parameters.zoomLimits).on("zoom", zoomed);

 var xScale = d3.scaleLinear().range([parameters.margin.l, w - parameters.margin.r]);
 var yScale = d3.scaleLinear().range([h - parameters.margin.b, parameters.margin.t]);
 var xAxis = d3.axisBottom(xScale)
 var yAxis = d3.axisLeft(yScale)

 d3.csv(CSV_PREFIX + CSV_URL, function (error_, data_) {

     if (error_) throw error_;

     setSelectors();

     data_.forEach(function (d_) {

         points[d_[disco.index]] = d_;
         limits.x.min = Math.min(limits.x.min, d_[disco.x] * 1.2);
         limits.x.max = Math.max(limits.x.max, d_[disco.x] * 1.2);
         limits.y.min = Math.min(limits.y.min, d_[disco.y] * 1.2);
         limits.y.max = Math.max(limits.y.max, d_[disco.y] * 1.2);

         d_.visibility = {
             color: 1,
             radius: 1,
             culture: 1
         };

         //debugging
         d_.debug = {};
         d_.debug.index = d_.indx;

     });

     loadAndProcessDataset();

 });

 function setSelectors() {

     var selector1 = document.getElementById("colorSelector");
     disco.color.forEach(function (d_) {

         var option = document.createElement("option");
         option.text = d_.option;
         option.value = d_.index;
         selector1.add(option);

     });

     var selector2 = document.getElementById("radiusSelector");
     disco.radius.forEach(function (d_) {

         var option = document.createElement("option");
         option.text = d_.option;
         option.value = d_.index;
         selector2.add(option);

     });

     document.getElementById("colorSelector").options.selectedIndex = 1;
     document.getElementById("radiusSelector").options.selectedIndex = 1;


 }

 function loadAndProcessDataset() {

     var queue = d3.queue();

     DATASET_URLS.forEach(function (url_) {
         queue.defer(d3.csv, DATASET_PREFIX + url_)
     });

     queue.awaitAll(function (error_, dataset_) {

         if (error_) throw error_;

         dataset_.forEach(function (d_, i_) {

             var name = DATASET_URLS[i_].replace(".csv", "").trim();
             db[name] = [];

             d_.forEach(function (e_) {

                 db[name][e_[Object.keys(e_)[0]]] = e_;

             });

         });

         loadReferencePoints();
	 
         restructurePoints(points, db);
         setCulturesPopup(db);
         makeEvenRadiuses(disco.radius);

         svg = d3.select("#scatter2D").append("svg").attr("width", w).attr("height", h).call(zoom);
         div = d3.select("#tooltip");

         var defs = svg.append("defs")
             .append("pattern")
             .attr("id", "cross")
             .attr("patternUnits", "userSpaceOnUse")
             .attr("x", 3)
             .attr("y", 2)
             .attr("width", 12)
             .attr("height", 12)
             .append("svg:path")
             .attr("d", "M 4 4 L 10 10 M 4 10 L 10 4")
             .attr("stroke", "#000000");

         var defs = svg.append("defs")
             .append("pattern")
             .attr("id", "mark")
             .attr("patternUnits", "userSpaceOnUse")
             .attr("x", 4)
             .attr("y", 3)
             .attr("width", 12)
             .attr("height", 12)
             .append("svg:path")
             .attr("d", "M 0 6 L 12 6 M 6 0 L 6 12")
             .attr("stroke", "#000000");

     })

 }

 function loadReferencePoints(){
     
      d3.csv(CSV_PREFIX + REFERENCE_URL, function (error_, data_) {

      if (error_) throw error_;
          
          data_.forEach(function(d_){ parameters.references.push(d_); })
          
          drawGridLines();
          drawPoints();
          drawReferences();
          renderLegend();

          d3.select(window).on('resize.updatesvg', updateOnResize);
          
      });
 }

 function updateOnResize() {

     w = window.innerWidth;
     h = window.innerHeight;

     svg.attr("width", w).attr("height", h);
     legendBox.attr("transform", function () {
         return "translate(" + (document.getElementById("ui").offsetLeft) + "," + (document.getElementById("ui").offsetTop + document.getElementById("ui").offsetHeight + 16) + ")";
     });

     xScale.range([parameters.margin.l, svg.attr("width") - parameters.margin.r]);
     yScale.range([svg.attr("height") - parameters.margin.b, parameters.margin.t]);

     drawGridLines();

     
     d3.selectAll(".nodes")
    .attr("cx", function (d_) {
         if (d_ != undefined) {
             return xScale(d_[disco.x]);
         }
     })
     .attr("cy", function (d_) {
         if (d_ != undefined) {
             return yScale(d_[disco.y]);
         }
     });
     
 }

 function makeXgrid(scale_) {
     return d3.axisBottom(scale_).ticks();
 }

 function makeYgrid(scale_) {
     return d3.axisLeft(scale_).ticks();
 }

 function zoomed() {

     var new_xScale = d3.event.transform.rescaleX(xScale)
     var new_yScale = d3.event.transform.rescaleY(yScale)

     gX.call(xAxis.scale(new_xScale));
     gY.call(yAxis.scale(new_yScale));

     tX.attr("x1", new_xScale(0));
     tX.attr("x2", new_xScale(0));
     tX.attr("opacity", function(d_){
                  
         if(new_xScale(0).between([parameters.margin.l, w - parameters.margin.r])) { return 1.0; }
         return 0.0;
         
     })
     
     tY.attr("y1", new_yScale(0));
     tY.attr("y2", new_yScale(0));
     tY.attr("opacity", function(d_){
         
         if(new_yScale(0).between([parameters.margin.t, h - parameters.margin.b])) { return 1.0; }
         return 0.0;
         
     })

     var dims = {
	 
         x: {
             min: new_xScale.domain()[0],
             max: new_xScale.domain()[1]
         },
         y: {
             min: new_yScale.domain()[0],
             max: new_yScale.domain()[1]
         }
     };

         d3.selectAll(".references").attr("transform", function(d_){ return "translate(" + new_xScale(d_.score_1) + "," + new_yScale(d_.score_2) + ")rotate(45)"; })
         d3.selectAll(".nodes").attr("transform", d3.event.transform).attr("visibility", function(d_) { 
             
             if(d_ != undefined){
             if(d_[disco.x] < dims.x.min || d_[disco.x] > dims.x.max ||  d_[disco.y] < dims.y.min || d_[disco.y] > dims.y.max || !checkRadiusVisibility(d_.visibility) ) { return "hidden"; } return true; 
             }
         
             })
             .attr("r", function(d_) { 
             
             if(d_ != undefined){
             var r = parameters.radius.index.split("."); 
             return parameters.radius.f(d_[r[0]][r[1]], "r") / d3.event.transform.k; 
         
             }
             })
             .style("stroke-width", 5 / d3.event.transform.k);

 }


 function drawGridLines() {

     d3.selectAll(".support").remove();

     xScale.domain([limits.x.min, limits.x.max]);
     yScale.domain([limits.y.min, limits.y.max]);

     tX = svg.append('line').attr("class", "zero support").attr("x1", xScale(0)).attr("y1", parameters.margin.t).attr("x2", xScale(0)).attr("y2", h - parameters.margin.b)
     .attr("opacity", function(d_){
         
         if(xScale(0).between([parameters.margin.l, w - parameters.margin.r])) { return 1.0; }
         return 0.0;
         
     })
     .attr("stroke", "#DEDEDE");
     
     tY = svg.append('line').attr("class", "zero support").attr("x1", parameters.margin.l).attr("y1", yScale(0)).attr("x2", w - parameters.margin.r).attr("y2", yScale(0))
    .attr("opacity", function(d_){
         
         if(yScale(0).between([parameters.margin.t, h - parameters.margin.b])) { return 1.0; }
         return 0.0;
         
     })
    .attr("stroke", "#DEDEDE");

     gX = svg.append('g').attr("class", "axis axis--x support")
         .attr('transform', 'translate(' + 0 + ',' + (h - parameters.margin.b) + ')')
         .call(xAxis);
     
     gY = svg.append('g').attr("class", "axis axis--y support")
         .attr('transform', 'translate(' + parameters.margin.l + ',' + 0 + ')')
         .call(yAxis);

     labelX = svg.append("text")
         .attr("class", "support")
         .attr("transform", "translate(" + (parameters.margin.l + (w - parameters.margin.r - parameters.margin.l) / 2) + "," + (h - parameters.margin.b + 48) + ")")
         .attr("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .text(disco.xAlias)

     labelY = svg.append("text")
         .attr("class", "support")
         .attr("transform", "translate(" + (parameters.margin.l - 40) + "," + (parameters.margin.t + (h - parameters.margin.t - parameters.margin.b) / 2) + ") rotate(-90)")
         .attr("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .text(disco.yAlias)

     var title = svg.append("text")
                 .attr("class", "support")
                 .attr("transform", "translate(" + parameters.margin.l + ", 45)")
                 .attr("font-size", 24)
                 .attr("font-family", "sans-serif")
                 .text(disco.title);
     
 }

 function renderLegend() {

     d3.selectAll(".legend").remove();

     legendParameters = {

         x: document.getElementById("ui").offsetLeft,
         y: document.getElementById("ui").offsetTop + document.getElementById("ui").offsetHeight + 16,
         width: document.getElementById("ui").offsetWidth,
         height: 160,
         fontSize: "10px",
         header1: {
             x: 16,
             y: 24
         },
         header2: {
             x: 16,
             y: 74
         },
         header3: {
             x: 16,
             y: 120
         },
         carriage: 0,

         maxLength: 20,
         separation: 12,
         increment: 12,

     }

     legendBox = svg.append("g").attr("id", "legendBox").attr("transform", function () {
         return "translate(" + (legendParameters.x) + ", " + legendParameters.y + ")";
     })

     var legendBackground = legendBox.append("rect")
         .attr("class", "legendBox legend legendBackground")
         .attr("x", 0)
         .attr("y", 0)
         .attr("width", legendParameters.width)
         .attr("height", legendParameters.height)

     var cLabel = legendBox.append("text").attr("class", "legend").attr("transform", "translate(0," + (legendParameters.header1.y - legendParameters.separation) + ")").style("font-family", "sans-serif").attr("font-size", legendParameters.fontSize).attr("font-weight", "bold").each(function (d_) {

         var textX = 0;
         var lines = splitToMultiLines(parameters.color.option, legendParameters.maxLength).split('\n');
         for (var i = 0; i < lines.length; i++) {
             d3.select(this)
                 .append("tspan")
                 .attr("dx", legendParameters.header1.x)
                 .attr("dy", legendParameters.separation)
                 .attr("x", textX)
                 .text(lines[i]);
         }
     });

     legendParameters.height += legendParameters.increment * (Math.floor(parameters.color.option.length / 20));
     legendParameters.header2.y += legendParameters.increment * (Math.floor(parameters.color.option.length / 20));
     legendParameters.header3.y += legendParameters.increment * (Math.floor(parameters.color.option.length / 20));

     var legendList = legendBox.selectAll(".legendList").data(parameters.color.dict)
         .enter()
         .append("g")
         .attr("transform", function (d_, i_) {
             return "translate(38," + (48 + i_ * 18 + (legendParameters.increment * (Math.floor(parameters.color.option.length / 20)))) + ")";
         })
         .attr("class", "legendList legend")

     legendList.append("rect")
         .attr("class", "legend")
         .attr("id", function (d_, i_) {
             return "blob_" + i_;
         }, true)
         .attr("x", -20)
         .attr("y", -11)
         .attr("rx", 6)
         .attr("ry", 6)
         .attr("width", 12)
         .attr("height", 12)
         .attr("stroke-width", 2.5)
         .attr("opacity", 0.69)
         .attr("fill", function (d_, i_) {

             legendParameters.height += d3.select(this).node().getBBox().height * 1.1;
             legendParameters.header2.y += d3.select(this).node().getBBox().height * 1.1;
             legendParameters.header3.y += d3.select(this).node().getBBox().height * 1.1;

             if (parameters.color.index == "na") {
                 return parameters.na_color;
             }
             if (Array.isArray(parameters.color.colors)) {

                 return parameters.color.colors[i_];

             } else {

                 return parameters.color.colors(i_);

             }

         })

     legendList.append("text")
         .attr("class", function (d_, i_) {
             return "legend colorLabel_" + i_
         })
         .attr("baseline", "central")
         .style("font-family", "sans-serif")
         .style("font-size", legendParameters.fontSize)
         .text(function (d_) {

             //convert 0,1 to FALSE, TRUE for
             //ornaments and tensions
             if (d_.l == "0") {
                 return "FALSE";
             }
             if (d_.l == "1") {
                 return "TRUE";
             }
             return d_.l;

         });

     legendList.append("rect")
         .attr("class", "legend")
         .attr("x", -26)
         .attr("y", -14)
         .attr("width", legendParameters.width - 16)
         .attr("height", 18)
         .attr("fill", "transparent")
         .on("mouseover", function (d_, i_) {

             d3.select(".colorLabel_" + i_).attr("font-weight", "bolder");

         })
         .on("mouseout", function (d_, i_) {

             d3.select(".colorLabel_" + i_).attr("font-weight", "normal");

         })
         .on("click", function (d_, i_) {

             var r = d3.select("#blob_" + i_);

             if (r.attr("fill") == "#DEDEDE") {

                 col = 0;

                 if (parameters.color.index != "na ") {

                     if (Array.isArray(parameters.color.colors)) {

                         col = parameters.color.colors[i_];

                     } else {

                         col = parameters.color.colors(i_);

                     }

                 } else {
                     col = parameters.na_color;
                 }


                 r.attr("fill", col);
                 d3.selectAll(".color_" + i_).style("visibility", function (d_) {


                     d_.visibility.color = 1;
                     return "visible"


                 });

             } else {

                 r.attr("fill", "#DEDEDE");
                 d3.selectAll(".color_" + i_).style("visibility", function (d_) {


                     d_.visibility.color = 0;
                     return "hidden";


                 });

             }

         })

     var rLabel = legendBox.append("text").attr("class", "legend").attr("transform", "translate(0," + (legendParameters.header2.y - legendParameters.separation) + ")").style("font-family", "sans-serif").attr("font-size", legendParameters.fontSize).attr("font-weight", "bold").each(function (d_) {

         var textX = 0;
         var lines = splitToMultiLines(parameters.radius.option, legendParameters.maxLength).split('\n');
         for (var i = 0; i < lines.length; i++) {
             d3.select(this)
                 .append("tspan")
                 .attr("dx", legendParameters.header2.x)
                 .attr("dy", legendParameters.separation)
                 .attr("x", textX)
                 .text(lines[i]);
         }
     });

     legendParameters.carriage = legendParameters.header2.y + 8 + legendParameters.increment * (Math.floor(parameters.radius.option.length / 20));
     legendParameters.height.y += 16;
     legendParameters.header2.y += 16;

     var legendList2 = legendBox.selectAll(".legendList2").data(parameters.radius.dict)
         .enter()
         .append("g")
         .attr("transform", function (d_) {
             legendParameters.carriage += d_.r * 2.2;
             return "translate(56," + (legendParameters.carriage) + ")";
         })
         .attr("class", "legendList2 legend")

     legendList2.append("rect")
         .attr("class", "legend")
         .attr("id", function (d_, i_) {

             return "rad_" + i_;
         })
         .attr("x", function (d_) {

             return -20 - opticalCompensation(d_.r);

         })
         .attr("y", function (d_) {

             return -d_.r;

         })
         .attr("rx", function (d_) {
             return d_.r;

         })
         .attr("ry", function (d_) {

             return d_.r;

         })
         .attr("width", function (d_) {

             return d_.r * 2;

         })
         .attr("height", function (d_) {

             return d_.r * 2;

         })
         .attr("fill", "#FFFFFF")
         .attr("stroke", "#000000")
         .attr("transform", function (d_) {

             legendParameters.height += d3.select(this).node().getBBox().height * 1.1;
             legendParameters.header3.y += d3.select(this).node().getBBox().height * 1.1;

         });

     legendList2.append("text")
         .attr("class", function (d_, i_) {
             return "legend radiusLabel_" + i_;
         })
         .attr("id", function (d_, i_) {
             return "scale_" + i_;
         })
         .attr("baseline", "central")
         .attr("dx", -16)
         .attr("dy", 3)
         .style("font-family", "sans-serif")
         .style("font-size", legendParameters.fontSize)
         .text(function (d_) {
             return d_.l;
         });

     legendList2.append("rect")
         .attr("class", "legend")
         .attr("x", -48)
         .attr("y", function (d_) {
             return -d_.r * 1.1;
         })
         .attr("width", legendParameters.width - 16)
         .attr("height", function (d_) {
             return d_.r * 2.2;
         })
         .attr("fill", "transparent")
         .on("mouseover", function (d_, i_) {

             d3.select(".radiusLabel_" + i_).attr("font-weight", "bolder");

         })
         .on("mouseout", function (d_, i_) {

             d3.select(".radiusLabel_" + i_).attr("font-weight", "normal");

         })
         .on("click", function (d_, i_) {


             var r = d3.select("#rad_" + i_);

             if (r.attr("fill") == "transparent") {

                 r.attr("fill", "#FFFFFF")
                 r.attr("stroke", "#000000");
                 d3.selectAll(".radius_" + i_).style("visibility", function (d_) {


                     d_.visibility.radius = 1;
                     return "visible"


                 });

             } else {

                 r.attr("fill", "transparent");
                 r.attr("stroke", "#DEDEDE");

                 d3.selectAll(".radius_" + i_).style("visibility", function (d_) {


                     d_.visibility.radius = 0;
                     return "hidden"


                 });

             }


         });

     var bLabel = legendBox.append("text").attr("class", "legend").attr("dx", legendParameters.header3.x).attr("dy", legendParameters.header3.y).style("font-family", "sans-serif").attr("font-size", legendParameters.fontSize).attr("font-weight", "bold").text("Societies");

     var legendList3 = legendBox.selectAll(".legendList3").data(disco.checkbox.legend)
         .enter()
         .append("g")
         .attr("transform", function (d_, i_) {
             return "translate(38," + (legendParameters.header3.y + 24 + i_ * 18) + ")";
         })
         .attr("class", "legendList3 legend")

     legendList3.append("rect")
         .attr("class", "legend")
         .attr("id", function (d_, i_) {
             return "blob_" + i_;
         }, true)
         .attr("x", -20)
         .attr("y", -9)
         .attr("rx", 6)
         .attr("ry", 6)
         .attr("width", 12)
         .attr("height", 12)
         .attr("stroke-width", 2.5)
         .attr("opacity", 0.69)
         .attr("fill", "url(#cross)")
         .attr("stroke", "#000000")
         .attr("stroke-width", 1.2)

     legendList3.append("text")
         .attr("class", "legend checkLabel")
         .attr("baseline", "central")
         .style("font-family", "sans-serif")
         .style("font-size", legendParameters.fontSize)
         .text(function (d_) {
             return d_;
         });

     legendList3.append("rect")
         .attr("class", "legend")
         .attr("x", -26)
         .attr("y", -15)
         .attr("width", 128)
         .attr("height", 24)
         .attr("fill", "transparent")
         .on("mouseover", function (d_) {

             d3.select(".checkLabel").attr("font-weight", "bolder");

         })
         .on("mouseout", function (d_) {

             d3.select(".checkLabel").attr("font-weight", "normal");

         })
         .on("click", function (d_) {

             var p = document.getElementById("culturesPopup"); 
             p.style.display = "grid";
	     p.style.zIndex = maxZIndex();
	     
         });

     d3.select(".legendBackground").attr("height", legendParameters.height)

 }

 function drawPoints() {

     d3.selectAll(".nodes").remove();

     var sortR = parameters.radius.index.split(".")

     var RenderedPoints = svg.selectAll(".nodes").data(points.sort(function (x_, y_) {
         if(parameters.radius.index != "na") { return d3.descending(x_[sortR[0]][sortR[1]], y_[sortR[0]][sortR[1]]) }
     }));

     RenderedPoints.enter()
         .append("circle")
         .attr("class", function (d_) {

          var c = parameters.color.index.split(".");
          var r = parameters.radius.index.split(".");
         
          if (d_ != undefined) {

              
                 var colorIndex = 0;
                 var radiusIndex = 0;
                 
                 if(parameters.color.index != "na"){
                     
                     var c = parameters.color.index.split(".");
                     colorIndex = parameters.color.f(d_[c[0]][c[1]]);
                     
                 }
                    
              
                 if(parameters.radius.index != "na"){
                     
                     var c = parameters.radius.index.split(".");
                     radiusIndex = parameters.radius.f(d_[r[0]][r[1]], "i");
                     
                 }
                     
                 var cls = "nodes" + " indx_" + d_.indx + " color_" + colorIndex + " radius_" + radiusIndex + " culture_" + disco.checkbox.cultures[d_.nhs_region].culture.replace(" ", "_").replace("'", "_");
                 return cls;

             }
         
         })
         .merge(RenderedPoints)
         .attr("cx", function (d_) {
             if (d_ != undefined) {
                 return xScale(d_[disco.x]);
             }
         })
         .attr("cy", function (d_) {
             if (d_ != undefined) {
                 return yScale(d_[disco.y]);
             }
         })
         .attr("r", function (d_, i_) {

             if (d_ != undefined) {

                 if(parameters.radius.index != "na"){
                 var r = parameters.radius.index.split(".");
                 return parameters.radius.f(d_[r[0]][r[1]], "r");
                 }else{
   
                     return parameters.radius.f(0, "r");
                     
                 }


             }

         })
         .attr("fill", function (d_, i_) {

             if (d_ != undefined) {

                 if (parameters.color.index == "na") {
                     return parameters.na_color;
                 }
                 var c = parameters.color.index.split(".");

                 if (Array.isArray(parameters.color.colors)) {
                     return JSON.parse(d_.highlighted) ? d3.rgb(parameters.color.colors[parameters.color.f(d_[c[0]][c[1]])]).darker(parameters.highlightedDarker) : parameters.color.colors[parameters.color.f(d_[c[0]][c[1]])] ;
                 } else {
                     return JSON.parse(d_.highlighted) ? d3.rgb(parameters.color.colors(parameters.color.f(d_[c[0]][c[1]]))).darker(parameters.highlightedDarker) : parameters.color.colors(parameters.color.f(d_[c[0]][c[1]]));
                 }

             }

         })
         .attr("opacity", function (d_, i_) {
         
             if (d_ != undefined) { return JSON.parse(d_.highlighted) ? parameters.highlightedAlpha :  parameters.defaultAlpha; }
             return parameters.defaultAlpha;         
	     
         })
         .on("mouseover", function (d_) {

             d3.select(this).attr("stroke-width", 5).attr("stroke", "#DEDEDE");

         })
         .on("mouseout", function (d_) {

             d3.select(this).attr("stroke", "none");

         })
         .on("click", function (d_, i_) {

             var ci = parameters.color.index.split(".");
             var ri = parameters.radius.index.split(".");
             d_.debug.x = d_[disco.x];
             d_.debug.y = d_[disco.y];
             console.log(d_.debug);

             document.getElementById("audio").src = disco.audio_prefix + d_.indx + ".wav";

             disco.popup.forEach(function (p_) {

                 var indices = p_.index.split(".")

                 document.getElementById(indices[1]).innerHTML = "<b>" + disco.checkbox.dict[indices[1]] + "</b>:<br> " + (indices[1] == "permalink" ? "<a  class=\"\" href=\"" + d_[indices[0]][indices[1]] +  "\" target=\"_blank\" style=\"text-decoration:none;color:#000000\">" + d_[indices[0]][indices[1]] + "</a>" : d_[indices[0]][indices[1]]);


             })

             maximizePopup();
         
             div.style("left", function () {
                     return window.innerWidth / 2 - 200;
                 })
                 .style("top", function () {
                     return window.innerHeight / 2 - 150
                 })
                 .style("pointer-events", "all");
             div.transition()
                 .duration(500)
                 .style("opacity", 0.95);

         });

     RenderedPoints.exit().remove();

 }

 function drawReferences() {

     d3.selectAll(".references").remove();

     var ReferencePoints = svg.selectAll(".references").data(parameters.references);
     
     ReferencePoints.enter()
         .append("rect")
         
         .attr("class", function (d_, i_) { return "references " + "reference_" + i_; })
         .merge(ReferencePoints)
         .attr("transform", function(d_){ return "translate(" + xScale(d_.score_1) + "," + yScale(d_.score_2) + ")rotate(45)"; })
         .attr("width", parameters.referenceDimensions.width)
         .attr("height", parameters.referenceDimensions.height)
         .attr("fill", function(d_, i_){ return parameters.referenceColorsFromData ? d_.hex : d3.rgb(parameters.color.colors[i_]).darker(parameters.referenceDarkerRatio); })
         .attr("stroke", function(d_, i_){ return parameters.referenceColorsFromData ? d_.hex : parameters.color.colors[i_]; })
         .attr("stroke-width", 2)
         .attr("stroke-opacity", 0.75)
         .attr("fill-opacity", 0.5)
         .on("mouseover", function (d_) {

            d3.select(this).attr("stroke-width", 5).attr("stroke", "#DEDEDE"); 
         
         })
         .on("mouseout", function (d_, i_) {

            d3.select(this).attr("stroke-width", 2).attr("stroke", parameters.referenceColorsFromData ? d_.hex : parameters.color.colors[i_]);
         
         })
         .on("click", function (d_, i_) {

            minimizePopup();
            document.getElementById("type").innerHTML = "<b>" + d_.popup + "</b>"
         
            div.style("left", function () {
                     return window.innerWidth / 2 - 200;
                 })
                 .style("top", function () {
                     return window.innerHeight / 2 - 150
                 })
                 .style("pointer-events", "all");
             div.transition()
                 .duration(500)
                 .style("opacity", 0.95);

         })

     ReferencePoints.exit().remove();

 }


 function minimizePopup(){

     div.style("grid-template-areas", "'aux aux aux aux aux x' 'typ typ typ typ typ typ'");
     parameters.popup.forEach(function(d_){ div.select("#" + d_).style("display", "none"); });
     
 }

 function maximizePopup(){
     
     div.style("grid-template-areas", "'aux aux aux aux aux x' 'typ typ typ typ typ typ' 'yr yr yr yr yr yr' 'cnm cnm cnm cnm cnm cnm' 'ct ct ct ct ct ct' 'pln pln pln pln pln pln' 'hr hr hr hr hr hr' 'hs hs hs hs hs hs' 'clt clt clt clt clt clt'");
     parameters.popup.forEach(function(d_){ div.select("#" + d_).style("display", "inline"); })
     
 }

 function checkRadiusVisibility(vis_) {

     if (vis_.color == 0 || vis_.radius == 0 || vis_.culture == 0) {
         return false;
     } else {
         return true;
     }

 }

 function splitToMultiLines(str_, width_, brk_, cut_) {

     brk_ = brk_ || '\n';
     width_ = width_ || 75;
     cut_ = cut_ || false;
     if (!str_) {
         return str_;
     }
     var regex = '.{1,' + width_ + '}(\\s|$)' + (cut_ ? '|.{' + width_ + '}|.+$' : '|\\S+?(\\s|$)');
     return str_.match(RegExp(regex, 'g')).join(brk_);

 }

 function backToOrigin() { svg.call(zoom.transform, d3.zoomIdentity); }

 function makeEvenRadiuses(obj_) {

     obj_.forEach(function (d_) {

         d_.dict.forEach(function (k_, i_) {
             k_.r = parameters.radiusScale(i_ + 1);
         });
     });

 }

 function setCulturesPopup(db_) {

     //cultures NHS-R01: {culture: "Saramaka", region: "South America", sub: "Amazon and Orinoco"}
     //regions NHS-R01: {region: "South America", sub: "Amazon and Orinoco"}

     for(var key in points){
         
         disco.checkbox.cultures[points[key].nhs_region] = {culture: points[key]["NHSDiscography_Metadata"].culture, region: points[key]["NHSDiscography_Metadata"].hraf_region, sub: points[key]["NHSDiscography_Metadata"].hraf_subregion };
         
         disco.checkbox.regions[points[key].nhs_region] = {region: points[key]["NHSDiscography_Metadata"].hraf_region, sub: points[key]["NHSDiscography_Metadata"].hraf_subregion};
         
         if(disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region] == undefined){
            
            disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region] = [];
            
                disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion] = [];
                
                disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion].push({culture: points[key]["NHSDiscography_Metadata"].culture, id_hraf: points[key]["NHSDiscography_Metadata"].id_hraf, region: points[key]["NHSDiscography_Metadata"].hraf_region, sub: points[key]["NHSDiscography_Metadata"].hraf_subregion });
    
         }else{
            
            if(disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion] == undefined){
                
                disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion] = [];
                
                disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion].push({culture: points[key]["NHSDiscography_Metadata"].culture, id_hraf: points[key]["NHSDiscography_Metadata"].id_hraf, region: points[key]["NHSDiscography_Metadata"].hraf_region, sub: points[key]["NHSDiscography_Metadata"].hraf_subregion });
 
            }else{
                
                if(!isCultureAlreadyInList(disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion], points[key]["NHSDiscography_Metadata"].culture)){
                    
                    disco.checkbox.index[points[key]["NHSDiscography_Metadata"].hraf_region][points[key]["NHSDiscography_Metadata"].hraf_subregion].push({culture: points[key]["NHSDiscography_Metadata"].culture, id_hraf: points[key]["NHSDiscography_Metadata"].id_hraf, region: points[key]["NHSDiscography_Metadata"].hraf_region, sub: points[key]["NHSDiscography_Metadata"].hraf_subregion });
                                
                }
                
            }
            
         }
         
         
     }

     buildCultuesPopup();

 }

 function isCultureAlreadyInList(array_, culture_){
    
    var found = false;
    array_.forEach(function(d_){ if(d_.culture == culture_) { found = true; } })
    return found;
    
 }

 function buildCultuesPopup() {

     //button
     var btn = document.createElement("BUTTON");
     var t = document.createTextNode("select all");

     btn.appendChild(t);
     btn.setAttribute("onClick", "javascript: selectAll();");

     document.getElementById("selectAll").appendChild(btn);


     var und = "";


     for (var key in disco.checkbox.index) {

         if (key != "undefined") {

             var el0 = [];

             for (var key2 in disco.checkbox.index[key]) {

                 el0 += "check_" + key2.replace(" ", "_") + ",";

                 disco.checkbox.index[key][key2].forEach(function (tmp_) {

                     el0 += "check_" + tmp_.culture.replace(/['()]/g, '_') + ",";

                 });


             }

             el0 = el0.replace(/.$/, "");
             var str = "";
             str += "<ul><li><h3><input type=\"checkbox\" class=\"checkbox\" id=\"" + el0 + "\"name=\"check_" + key.replace(" ", "_") + "\" value=\"1\" onclick=\"toggleCheckbox(this)\" checked>" + key + "</h3><ul>";

             for (var key2 in disco.checkbox.index[key]) {

                 var el1 = "";

                 disco.checkbox.index[key][key2].forEach(function (tmp_) {

                     el1 += "check_" + tmp_.culture.replace(" ", "_") + ",";

                 })

                 el1 = el1.replace(/.$/, "");

                 str += "<li><input class=\"checkbox\" type=\"checkbox\" name=\"check_" + key2.replace(" ", "_") + "\" value=\"1\" onclick=\"toggleCheckbox(this)\" id=\"" + el1 + "\" style=\"display: none;\" checked>" + "<ul>";

                 disco.checkbox.index[key][key2].forEach(function (c_) {

                     str += "<li><input class=\"checkbox\" type=\"checkbox\" id=\"\" name=\"check_" + c_.culture.replace(/['()]/g, '_') + "\" value=\"1\" onclick=\"toggleCheckbox(this)\" checked>" + c_.culture + "</li>";

                 })

                 str += "</ul></li>";

             }

             str += "</ul>";
             document.getElementById(key.replace(/ /g, "_")).innerHTML = str;


         } else {

             und += "<li><h3><input type=\"checkbox\" class=\"checkbox\" name=\"check_" + key.replace(" ", "_") + "\" value=\"1\" checked><span style=\"color:darkred;\">" + key + "</font></h3><ul>";

             und += "</ul></li>";

         }
     }


     document.getElementById("Middle_East").innerHTML += und;

 }

 function toggleCheckbox(e_) {

     e_.value *= -1;
     if (e_.id != "") {


         var nm = e_.id;
         var children = nm.split(",");

         for (var i = 0; i < children.length; i++) {

             document.getElementsByName(children[i])[0].value = e_.value;
             document.getElementsByName(children[i])[0].checked = e_.checked;

         }

     }


     checkCulturesVisibility();

 }

 function selectAll() {

     var cb = document.getElementsByClassName("checkbox");
     for (var i = 0, n = cb.length; i < n; i++) {
         cb[i].checked = true;


         var cls = ".culture_" + cb[i].name.replace("check_", "").replace(/['()]/g, '_')

         d3.selectAll(cls).attr("visibility", function (d_) {

             d_.visibility.culture = 1;
             return "visible";

         })

     }

 }

 function checkCulturesVisibility() {

     var cb = document.getElementsByClassName("checkbox");
     for (var i = 0, n = cb.length; i < n; i++) {

         var name = cb[i].name.replace("check_", "").replace(" ", "_").replace("'", "_").replace("(", "_").replace(")", "_");
         var cls = ".culture_" + name;

         if (cb[i].value == 1) {

             d3.selectAll(cls).attr("visibility", function (d_) {

                 d_.visibility.culture = 1;
                 return "visible";

             })


         } else {

             d3.selectAll(cls).attr("visibility", function (d_) {

                 d_.visibility.culture = 0;
                 return "hidden";

             })



         }

     }

 }

 function changeColors() {

     parameters.color = getElementByName(disco.color, document.getElementById("colorSelector").value);
     for (var key in points) {
         points[key].visibility.color = 1;
     }
     
     //change colors
     d3.selectAll(".nodes").attr("fill", function (d_, i_) {

         if (d_ != undefined) {


             if (parameters.color.index == "na") {
                 return parameters.na_color;
             }
             var c = parameters.color.index.split(".");

             if (Array.isArray(parameters.color.colors)) {
                 return parameters.color.colors[parameters.color.f(d_[c[0]][c[1]])];
             } else {
                 return parameters.color.colors(parameters.color.f(d_[c[0]][c[1]]));
             }

         }

     });
     
     renderLegend();

 }

 function changeRadiuses() {

     parameters.radius = getElementByName(disco.radius, document.getElementById("radiusSelector").value);
     for (var key in points) {
         points[key].visibility.radius = 1;
     }     
     
     //change radiuses    
     d3.selectAll(".nodes").attr("r", function (d_, i_) {

         if (d_ != undefined) {

             if(parameters.radius.index != "na"){
             var r = parameters.radius.index.split(".");
             return parameters.radius.f(d_[r[0]][r[1]], "r");
             }else{

                 return parameters.radius.f(0, "r");

             }


         }

     });

     renderLegend();

 }


 function closePopup() {

     document.getElementById("culturesPopup").style.display = "none";
     document.getElementById("ui").style.zIndex = maxZIndex();
     rendering = true;

 }

 function getElementByName(array_, name_) {

     if (array_.filter(e_ => e_.index === name_).length > 0) {
         return array_.filter(e_ => e_.index === name_)[0];
     }

 }

 function restructurePoints(points_, db_) {

     points_.forEach(function (p_) {

         Object.keys(disco.indices).forEach(function (i_) {

             if (!disco.indices[i_].includes('.')) {

                 p_[i_] = db_[i_][p_["indx"]];

             } else {

                 var indices = disco.indices[i_].split(".");
                 p_[i_] = db_[i_.toString()][p_[indices[0]][indices[1]]];


             }

         })

     });

 }

 function closeTooltip() {

     div.transition()
         .duration(500)
         .style("opacity", 0.0);
     div.style("pointer-events", "none");

     document.getElementById("audio").pause();
     document.getElementById("ui").style.zIndex = maxZIndex();

 }

 function opticalCompensation(d_) { return (15 + d_ * 5) * 0.35; }

 function decimals(n_) {

     if (n_ > 100) {
         return 0
     } else if (n_ > 1) {
         return 1;
     } else {
         return 2;
     }

 }

 function fraction(n_) {

     if (n_ != Math.ceil(n_)) {

         var str = n_ + "";
         var parts = str.split(".");
         if (parts[0].length > 0 && parts[0] != "0") {
             return 1E-1;
         } else {

             var i = 0;
             while (parts[1].charAt(i) == "0") {

                 i++;
             }

             return Number("1E-" + (i + 2));

         }


     } else {

         if (n_ > 1 && n_ < 10) {
             return 1E-1;
         } else if (n_ == 1) {
             return 1E-2;
         } else {
             var str = n_ + "";
             return Number("1E" + (str.length - 3));
         }

     }

 }

 function showInfoBox() {

     document.getElementById("infoPopup").style.display = "grid";
     document.getElementById("infoPopup").style.zIndex = maxZIndex();
     rendering = false;

 }

 function closeInfo() {

     document.getElementById("infoPopup").style.display = "none";
     document.getElementById("ui").style.zIndex = maxZIndex();
     rendering = true;

 }

 function maxZIndex() {

     return Array.from(document.querySelectorAll("body *"))
           .map(a_ => parseFloat(window.getComputedStyle(a_).zIndex))
           .filter(a_ => !isNaN(a_))
           .sort()
           .pop();
 }

 function map(value_, min1_, max1_, min2_, max2_) {

     return min2_ + (value_ - min1_) / (max1_ - min1_) * (max2_ - min2_);

 }

 Number.prototype.between = function (domain_) {

     var min = Math.min.apply(Math, domain_);
     var max = Math.max.apply(Math, domain_);

     return this >= min && this <= max;

 };
