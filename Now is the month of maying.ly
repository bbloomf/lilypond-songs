\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Now is the month of maying"}}
  composer = \markup\oldStyleNum"Thomas Morley (1557–1602)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #8
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
  \key g \major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative g' {
  r2 r4 g | 
  \repeat volta 2 {
    g g a a | b2 b4 g | b4. a8 b4 cis |
    d2 d4 \bar "" \break a8 b | c4 c b g | a fis d d'8 c | b4 c a a
  }
  \alternative { { g2 r4 g } { g2 r4 b }}

  \break

  \repeat volta 2 {
    a d d cis d2 r4 a | c c b b | a2 r4 d8 c |
    b4 g d'2 | d,8 e fis g a b c4 | b4. c8 b4 a
  }
  \alternative { { g2 r4 b } { g1\fermata } }

  \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Now is the month of may -- ing,
  When mer -- ry lads are play -- ing,
  "" _ _ _ _ _ _ _ _
  _ _ _ _ _ _
  _
  Now
  ""
  Each with his bon -- ny lass
  Up -- on the gree -- ny grass.
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Each ""
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The Spring, clad all in glad -- ness
  Doth laugh at Win -- ter's sad -- ness,
  fa la la la la la la la la,
  fa la la la la la
  la,
  The
  la, 
  And to the bag -- pipe's sound
  The nymphs tread out their ground. 
  Fa la la la la, fa la la la la la la la, fa la la la. And la.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Fie then! why sit we mus -- sing
  Youth's sweet de -- light re -- fu -- sing? 
  "" _ _ _ _ _ _ _ _
  _ _ _ _ _ _
  _
  Fie
  ""
  Say, dain -- ty nymphs, and speak,
  Shall we play bar -- ley -- break?
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Say ""
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  r2 r4 d |
  \repeat volta 2 {
    e g g fis g2 g4 g g4. fis8 g4 g4 |
    fis2 fis4 f8 f e4. fis8 g4 g4 fis4 d8 e fis4 fis8 fis g4 g g fis |
  } 
  \alternative { { g2 r4 d } { g2 r4 d } } 
  \repeat volta 2 {
    f4 a a4. g8 fis2 r4 f | e e e e | e2 a8 g fis4 |
    d g2 d8 e | fis g a4 r e8 fis | g4 g g fis 
  }
  \alternative { { g2 r4 d } { g1\fermata } }
  \bar "|."
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
quintusMusic = \relative c' {
  r2 r4 b |
  \repeat volta 2 {
    c c c c | d2 d4 d4 | d4. d8 d4 g, |
    d'2 d4 d8 d | a4 c d e | a,2. d8 d | d4 e d4. c8 | 
  }
  \alternative { { b2 r4 b } { b2 r4 g } }
  \repeat volta 2 {
    a4 f' e e d2 r4 f,4 g a b b cis c?8 b a4 a |
    g4. a8 b4 b | a a8 g fis4 e | d g d'4. c8
  }
  \alternative { { b2 r4 g } { b1\fermata} } 
  \bar "|."
}
tenorMusic = \relative g {
  r2 r4 g |
  \repeat volta 2 {
    g g c, c g'2 g4 g | g4. d'8 b4 e |
    a,2 a4 a8 a | e4 a d, g | d'2. a8 a | b4 e, a d
  }
  \alternative { { d2 r4 g, }{ d'2 r4 d } }
  \repeat volta 2 {
    d4 d e a, | a2 r4 a e a a gis | a2 r2 |
    d8 c b4 g d'~ | d d,8 e fis g a4 | g2 r4 d'4 |
  }
  \alternative { { d2 r4 d } { d1\fermata}}
  \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c {
  r2 r4 g |
  \repeat volta 2 {
    c c a a | g2 g4 g | g'4. d8 g4 e |
    d2 d4 d8 d | c4 a b e | d2. d8 d | g4 c, d d |
  }
  \alternative { { g,2 r4 g } { g2 r4 g' } } 
  \repeat volta 2 {
    f4 d a' a | d,2 r4 d c a e' e a, a'8 g fis4 d |
    g4 g4 g,8 a b c | d2. a4 | b e d d |
  }
  \alternative { { g,2 r4 g' } { g,1\fermata}}
}
bassWords = \lyricmode {
}


altusWordsA = \lyricmode {

  \set stanza = "1."
  Now is the month of may -- ing,
  When mer -- ry lads are play -- ing,
  ""  _ _ _ _ _ _ _ _ _
  _ _ _ _ _ _ _ 
  Now
  ""
  Each with his bon -- ny lass
  Up -- on the gree -- ny grass.
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Each ""
}

altusWordsB = \lyricmode {
  \set stanza = "2."
  The Spring, clad all in glad -- ness
  Doth laugh at Win -- ter's sad -- ness,
  fa la la la la la la la la la, fa la la la la la la, 
  The
  la, 
  And to the bag -- pipe's sound
  The nymphs tread out their ground. 
  Fa la la la la, fa la la la la, fa la la la la la la.
  And la.
}

altusWordsC = \lyricmode {
  \set stanza = "3."
  Fie then! why sit we mus -- sing
  Youth's sweet de -- light re -- fu -- sing? 
  "" _ _ _ _ _ _ _ _ _
  _ _ _ _ _ _ _ 
  Fie
  ""
  Say, dain -- ty nymphs, and speak,
  Shall we play bar -- ley -- break?
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Say ""
}

quintusWordsA = \lyricmode {

  \set stanza = "1."
  Now is the month of may -- ing,
  When mer -- ry lads are play -- ing,
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Now
  ""
  Each with his bon -- ny lass
  Up -- on the gree -- ny grass.
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  Each ""
}

quintusWordsB = \lyricmode {
  \set stanza = "2."
  The Spring, clad all in glad -- ness
  Doth laugh at Win -- ter's sad -- ness,
  fa la la la la la la, fa la la la la la la,
  The
  la, 
  And to the bag -- pipe's sound
  The nymphs tread out their ground. 
  Fa la la la la, fa la la la, fa la la la la, fa la la la.
  And la.
}

quintusWordsC = \lyricmode {
  \set stanza = "3."
  Fie then! why sit we mus -- sing
  Youth's sweet de -- light re -- fu -- sing? 
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Fie
  ""
  Say, dain -- ty nymphs, and speak,
  Shall we play bar -- ley -- break?
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  Say ""
}

tenorWordsA = \lyricmode {

  \set stanza = "1."
  Now is the month of may -- ing,
  When mer -- ry lads are play -- ing,
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Now
  ""
  Each with his bon -- ny lass
  Up -- on the gree -- ny grass.
  "" _ _ _ _ _ _ _ _ _ _ _ _
  Each ""
}

tenorWordsB = \lyricmode {
  \set stanza = "2."
  The Spring, clad all in glad -- ness
  Doth laugh at Win -- ter's sad -- ness,
  fa la la la la la la, fa la la la la la la,
  The
  la, 
  And to the bag -- pipe's sound
  The nymphs tread out their ground. 
  Fa la la la la, __ fa la la la la la, fa la.
  And la.
}

tenorWordsC = \lyricmode {
  \set stanza = "3."
  Fie then! why sit we mus -- sing
  Youth's sweet de -- light re -- fu -- sing? 
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Fie
  ""
  Say, dain -- ty nymphs, and speak,
  Shall we play bar -- ley -- break?
  "" _ _ _ _ _ _ _ _ _ _ _ _
  Say ""
}

bassusWordsA = \lyricmode {

  \set stanza = "1."
  Now is the month of may -- ing,
  When mer -- ry lads are play -- ing,
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Now
  ""
  Each with his bon -- ny lass
  Up -- on the gree -- ny grass.
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  Each ""
}

bassusWordsB = \lyricmode {
  \set stanza = "2."
  The Spring, clad all in glad -- ness
  Doth laugh at Win -- ter's sad -- ness,
  fa la la la la la la, fa la la la la la la,
  The
  la, 
  And to the bag -- pipe's sound
  The nymphs tread out their ground. 
  Fa la la la la la, fa la la la la, fa la la la la la.
  And la.
}

bassusWordsC = \lyricmode {
  \set stanza = "3."
  Fie then! why sit we mus -- sing
  Youth's sweet de -- light re -- fu -- sing? 
  "" _ _ _ _ _ _ _ _ _ _ _ _ _
  Fie
  ""
  Say, dain -- ty nymphs, and speak,
  Shall we play bar -- ley -- break?
  "" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
  Say ""
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \set Staff.instrument = "Cantus"
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopranosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "sopranosIII"  \lyricsto "sopranos" \sopWordsIII
    
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altus"  \lyricsto "altos" \altusWordsA
    \new Lyrics = "altusB"  \lyricsto "altos" \altusWordsB
    \new Lyrics = "altusC"  \lyricsto "altos" \altusWordsC
    
   \new Staff = quintus <<
      \clef "treble_8"
      \new Voice = "quintus" { << \global \quintusMusic >> }
    >>
    \new Lyrics = "quintus"  \lyricsto "quintus" \quintusWordsA
    \new Lyrics = "quintusB"  \lyricsto "quintus" \quintusWordsB
    \new Lyrics = "quintusC"  \lyricsto "quintus" \quintusWordsC
    
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "tenor"  \lyricsto "tenors" \tenorWordsA
    \new Lyrics = "tenorB"  \lyricsto "tenors" \tenorWordsB
    \new Lyrics = "tenorC"  \lyricsto "tenors" \tenorWordsC
    
    \new Staff = bassus <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    \new Lyrics = "bassus"  \lyricsto "basses" \bassusWordsA
    \new Lyrics = "bassusB"  \lyricsto "basses" \bassusWordsB
    \new Lyrics = "bassusC"  \lyricsto "basses" \bassusWordsC
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      %\override VerticalAxisGroup #'staff-affinity = #0
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
}


\score {
  \unfoldRepeats
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \set Staff.instrument = "Cantus"
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "sopranosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "sopranosIII"  \lyricsto "sopranos" \sopWordsIII
    
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altus"  \lyricsto "altos" \altusWordsA
    \new Lyrics = "altusB"  \lyricsto "altos" \altusWordsB
    \new Lyrics = "altusC"  \lyricsto "altos" \altusWordsC
    
   \new Staff = quintus <<
      \clef "treble_8"
      \new Voice = "quintus" { << \global \quintusMusic >> }
    >>
    \new Lyrics = "quintus"  \lyricsto "quintus" \quintusWordsA
    \new Lyrics = "quintusB"  \lyricsto "quintus" \quintusWordsB
    \new Lyrics = "quintusC"  \lyricsto "quintus" \quintusWordsC
    
    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "tenor"  \lyricsto "tenors" \tenorWordsA
    \new Lyrics = "tenorB"  \lyricsto "tenors" \tenorWordsB
    \new Lyrics = "tenorC"  \lyricsto "tenors" \tenorWordsC
    
    \new Staff = bassus <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    \new Lyrics = "bassus"  \lyricsto "basses" \bassusWordsA
    \new Lyrics = "bassusB"  \lyricsto "basses" \bassusWordsB
    \new Lyrics = "bassusC"  \lyricsto "basses" \bassusWordsC
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}


