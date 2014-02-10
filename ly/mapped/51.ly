\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #51
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key aes \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  \repeat unfold 2 {
    ees4 |
    aes c ees8[ des] |
    
    c4 aes aes |
    bes des8[ c] bes[ aes] |
    g4 ees ees |
    aes4 c8[ bes] aes[ g] |
    f4 des f |
    
    ees4 aes g |
    %\partial 2
    aes2
  }
  %\partial 4
  c8[ des] |
  ees4 c8[ des] ees[ f] |
  ees4 des c |
  des bes8[ c] des[ ees] |
  des4 c bes |
  
  c4 aes8[ bes] c[ des] |
  c4 bes aes |
  g ees' d |
  ees4 b\rest ees, |
  aes c ees8[ des] |
  c4 aes aes |
  bes4 des8[ c] bes[ aes] |
  g4 ees ees |
  aes c8[ bes] aes[ g] |
  f4 des f |
  ees aes g |
  aes2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The ash grove, how grace -- ful, how plain -- ly ’tis speak -- ing,
  The wind through it play -- ing has lan -- guage for me;
  
  When o -- ver its branch -- es the sun -- light is break -- ing,
  A host of kind fac -- es is gaz -- ing on me;
  
  The friends of my child -- hood a -- gain are be -- fore me,
  Fond mem -- o -- ries wak -- en, as free -- ly I roam;
  With soft whis -- pers lad -- en its leaves rus -- tle o’er me,
  The ash grove, the ash grove that shel -- tered my home.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  My laugh -- ter is o -- ver, my step los -- es light -- ness,
  Old coun -- try -- side mea -- sures steal soft on my ear;
  
  I on -- ly re -- mem -- ber the past and its bright -- ness,
  The dear ones I mourn for a -- gain gath -- er here.
  
  From out of the shad -- ows their lov -- ing looks greet me,
  And wist -- ful -- ly search -- ing the leaf -- y green dome,
  I find oth -- er fac -- es fond bend -- ing to greet me,
  The ash grove, the ash grove a -- lone is my home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    ees4 |
    c ees ees |
    
    ees ees f |
    f f f |
    ees ees ees |
    ees ees ees |
    des des des |
    c c ees8[ des] |
    c2
  }
  aes'8[ bes] |
  c4 aes aes |
  aes aes aes |
  f ees ees |
  ees ees ees |
  
  ees f f |
  f f f |
  g g f |
  g s ees |
  c ees ees |
  ees ees f |
  
  f f f |
  ees ees ees |
  ees ees ees |
  des des des |
  c c ees8[ des] |
  c2 \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat unfold 2 {
    ees,4 |
    aes aes g |
    
    aes c f, |
    des f des |
    ees g ees'8[ des] |
    c[ bes] aes4 c |
    des des des, |
    
    ees ees ees |
    aes2
  }
  r4 |
  r aes8[ bes] c[ des] |
  c4 bes aes |
  bes g8[ aes] bes[ c] |
  bes4 aes g |
  
  aes f8[ g] aes[ bes] |
  aes4 g f |
  ees ees bes' |
  ees, ees'8[ des c bes] |
  aes4 aes g |
  aes c f, |
  
  des f des |
  ees g ees'8[ des] |
  c[ bes] aes4 c |
  des des des, |
  ees ees ees |
  aes2 \bar"|."
}
bassWords = \lyricmode {
  \repeat unfold 23 \skip1
  \repeat unfold 23 \skip1
  Friends of __ ""
  \set ignoreMelismata = ##t
  \repeat unfold 22 \skip1
  With __ _ _ _
}
bassWordsII = \lyricmode {
  \repeat unfold 23 \skip1
  \repeat unfold 23 \skip1
  \set ignoreMelismata = ##t
  Out _ of the
  \repeat unfold 23 \skip1
  I __ _ _ _
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "basses" { << \global \bassMusic >> }
    >>
    \new Lyrics \lyricsto "basses" \bassWords
    \new Lyrics \lyricsto "basses" \bassWordsII
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Ash Grove"}}
  composer = \markup\oldStyleNum{"Welsh Folk Song," \italic"Llwyn Onn"}
  tagline = ""
}}


