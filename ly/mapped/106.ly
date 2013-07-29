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
       (padding . 0)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #106
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
  \key a \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  \times 2/3 {e8 e e} |
  cis'4 cis b8. a16 gis8. fis16 |
  e2. \times 2/3 {e8 e e} |
  d'4 cis b8. a16 gis8. a16 |
  
  b2. \times 2/3 {e,8 e e} |
  cis'4 cis b8. a16 gis8. a16 |
  fis2. \times 2/3 {fis'8 fis fis} |
  e4 r4 \bar""
  
  \times 2/3 {cis8 d cis} \times 2/3 {b cis b} |
  a2. b4\rest |
  \key e\major
  b4 b gis8. a16 b8. cis16 |
  b2 e4 b4\rest |
  dis8. cis16 b8. a16 fis4 b |
  
  %page2
  gis2. b4\rest |
  b b gis8. a16 b8. cis16 |
  b2 e4 b\rest |
  dis8. cis16 b8. a16 fis4 b |
  e,2 e |
  
  \key a\major
  cis'4 cis b8. a16 gis8. fis16 |
  e2. \times 2/3 {e8 e e} |
  d'4 cis b8. a16 gis8. a16 |
  b2. \times 2/3 {e,8 e e} |
  
  cis'4 cis b8. a16 gis8. a16 |
  fis2. \times 2/3 {fis'8 fis fis} |
  e4 r \times 2/3 {cis8 d cis} \times 2/3 {b cis b} |
  a2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Mer -- ri -- ly sing our hap -- py eve -- ning song, mer -- ri -- ly sing,
  Cheer -- i -- ly now the joy -- ful notes pro -- long; mer -- ri -- ly sing;
  Heart -- i -- ly join our cheer -- ful, hap -- py throng, mer -- ri -- ly sing,
  mer -- ri -- ly sing, mer -- ri -- ly, mer -- ri -- ly, mer -- ri -- ly sing.
  
  Chase a -- way all care and sad -- ness,
  Swell the an -- them loud and long;
  Lift your hearts to joy and glad -- ness
  \set associatedVoice = "sopranos"
  With the ech -- oes of our song;
  \dropLyricsXV
  Then sing \unset associatedVoice our hap -- py eve -- ning \raiseLyrics song, mer -- ri -- ly sing,
  Cheer -- i -- ly now the joy -- ful notes pro -- long, mer -- ri -- ly sing;
  Heart -- i -- ly join our cheer -- ful, hap -- py throng, mer -- ri -- ly sing,
  mer -- ri -- ly sing, mer -- ri -- ly, mer -- ri -- ly, mer -- ri -- ly sing.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Joy -- ful -- ly sing, the cho -- rus now we raise, mer -- ri -- ly sing,
  Crown -- ing the night with mu -- sic’s grand -- est lays; mer -- ri -- ly sing;
  Sing -- ing will bless and bright -- en all our days, mer -- ri -- ly sing,
  mer -- ri -- ly sing, mer -- ri -- ly, mer -- ri -- ly, mer -- ri -- ly sing.
  
  \set alignBelowContext = #"women"
  Mu -- sic is a gold -- en trea -- sure,
  Beau -- ty dwells in ev -- ’ry sound;
  Joy is found in ev -- ’ry mea -- sure,
  \unset alignBelowContext
  Let its plea -- sures now a -- bound;
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
  \times 2/3 {e8 e e} |
  e4 e fis8. fis16 e8. d16 |
  cis4 \times 2/3 {cis8 cis cis} cis4 \times 2/3 {e8 e e } |
  e4 e e8. e16 d8. cis16 |
  
  e4 \times 2/3 {e8 e e} e4 \times 2/3 {e8 e e} |
  e4 e d8. cis16 e8. e16 |
  d4 \times 2/3 {d8 d d} d4 \times 2/3 {a'8 a a} |
  a4 \times 2/3 {e8 e e}
  
  \times 2/3 {e fis e} \times 2/3 {d e d} |
  cis2. s4 |
  \key e\major
  gis'4 gis e8. fis16 gis8. a16 |
  gis2 gis4 s |
  fis8. a16 gis8. fis16 dis4 dis |
  
  %page2
  e2. s4 |
  gis4 gis e8. fis16 gis8. a16 |
  gis2 gis4 s |
  fis8. a16 gis8. fis16 dis4 dis |
  e2 d |
  
  \key a\major
  cis4 e fis8. fis16 e8. d16 |
  cis4 \times 2/3 {cis8 cis cis} cis4 \times 2/3 {e8 e e } |
  e4 e e8. e16 d8. cis16 |
  e4 \times 2/3 {e8 e e} e4 \times 2/3 {e8 e e} |
  
  e4 e d8. cis16 e8. e16 |
  d4 \times 2/3 {d8 d d} d4 \times 2/3 {a'8 a a} |
  a4 \times 2/3 {e8 e e} \times 2/3 {e fis e} \times 2/3 {d e d} |
  cis2. \bar"|."
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
  \times 2/3{e,8 e e} |
  e4 a a8. a16 a8. a16 |
  a4 \times 2/3{a8 a a} a4 \times 2/3{e8 e e} |
  gis4 a d8. cis16 b8. a16 |
  
  gis4 \times 2/3{gis8 gis gis} gis4 \times 2/3{e8 e e} |
  a4 a a8. a16 a8. a16 |
  a4 \times 2/3{a8 a a} a4 \times 2/3{d8 d d} |
  cis4 s
  
  s \times 2/3{gis8 gis gis} |
  a2. s4 |
  \key e\major
  gis4 b e e |
  e e b b |
  b b b b |
  
  %page2
  b b b s |
  e e e e |
  e e b b |
  b8. b16 b8. b16 b4 a |
  gis2 b |
  
  \key a\major
  a4 a a8. a16 a8. a16 |
  a4 \times 2/3{a8 a a} a4 \times 2/3{e8 e e} |
  gis4 a d8. cis16 b8. a16 |
  gis4 \times 2/3{gis8 gis gis} gis4 \times 2/3{e8 e e} |
  
  a4 a a8. a16 a8. a16 |
  a4 \times 2/3{a8 a a} a4 \times 2/3{d8 d d} |
  cis4 s s \times 2/3{gis8 gis gis} |
  a2. \bar"|."
}

tenorWords = \lyricmode {
  \repeat unfold 50 \skip1
  Chase a -- way all care and sad -- ness,
  Swell the an -- them loud and long;
  Lift your hearts to joy and glad -- ness
}

tenorWordsII = \lyricmode {
  \repeat unfold 50 \skip1
  Mu -- sic is a gold -- en trea -- sure,
  Beau -- ty dwells in ev -- ’ry sound;
  Joy is found in ev -- ’ry mea -- sure,
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \times 2/3{e,8 e e} |
  a,4 cis d8. d16 d8. d16 |
  a4 \times 2/3{a8 a a} a4 \times 2/3{e'8 e e} |
  e4 e e8. e16 e8. e16 |
  
  e4 \times 2/3{e8 e e} e4 \times 2/3{e8 e e} |
  a,4 a a8. a16 b8. cis16 |
  d4 \times 2/3{d8 d d} d4 \times 2/3{d8 d d} |
  e4 d\rest \bar""
  
  d\rest \times 2/3{e8 e e} |
  a,2. d4\rest |
  \key e\major
  e4 e e e |
  e e e e |
  b b b b |
  
  %page2
  e e e d\rest |
  e e e e |
  e e e e |
  b8. b16 b8. b16 b4 b |
  e2 e |
  
  \key a\major
  a,4 cis d8. d16 d8. d16 |
  a4 \times 2/3{a8 a a} a4 \times 2/3{a8 a a} |
  e'4 e e8. e16 e8. e16 |
  
  e4 \times 2/3{e8 e e} e4 \times 2/3{e8 e e} |
  a,4 a a8. a16 b8. cis16 |
  d4 \times 2/3{d8 d d} d4 \times 2/3{d8 d d} |
  e4 d\rest \bar""
  
  d\rest \times 2/3{e8 e e} |
  a,2. \bar"|."
}
bassWords = \lyricmode {
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
    \new Lyrics = "altos"  \lyricsto "altos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "altos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "altos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "tenorsI" \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
    \new Lyrics = "tenorsII" \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics = "tenorsIII" \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Merrily Sing"}}
  composer = \markup\oldStyleNum"James Henry Fillmore (1849–1936)"
  tagline = ""
}}


global = {
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	d'2 bes |
  f4 bes2 b4\rest |
  d2 bes |
  c4 c2 b4\rest |
  d2 bes |
  ees4 ees2 ees4 |
  d8 d4 d8 c4 c |
  bes2. b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Good night, la -- dies!
  Good night, la -- dies!
  Good night, la -- dies!
  We’re go -- ing to leave you now.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Fare -- well, la -- dies!
  Fare -- well, la -- dies!
  Fare -- well, la -- dies!
  We’re go -- ing to leave you now.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Sweet dreams, la -- dies!
  Sweet dreams, la -- dies!
  Sweet dreams, la -- dies!
  We’re go -- ing to leave you now.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f2 d |
  d4 d2 s4 |
  f2 d |
  f4 f2 s4 |
  f2 f4( aes) |
  g4 g2 g4 |
  f8 f4 d8 ees4 ees |
  d2. s4 \bar"|."
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
  bes2 f |
  f4 f2 s4 |
  bes2 f |
  a4 a2 s4 |
  bes2 bes |
  bes4 bes2 bes4 |
  bes8 bes4 bes8 a4 a |
  bes2. s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,2 bes |
  bes4 bes2 d4\rest |
  bes2 bes |
  f'4 f2 d4\rest |
  bes2 d |
  ees4 ees2 ees4 |
  f8 f4 f8 f,4 f |
  bes2. d4\rest \bar"|."
}
bassWords = \lyricmode {
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
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Good Night Ladies"}}
  %composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}


global = {
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  g'2 a |
  bes1 \bar"||"
  \mark"2"
  bes2 c |
  d2. g4 \bar"||"
  \mark"3"
  d g g f |
  g2 d4. c8 \bar"||"
  \mark"4"
  bes2 a |
  g1 \bar"|."
}
sopWords = \lyricmode {
  Oh my Love
  Lov’st thou me,
  then Quick -- ly come and save him who dies for thee.
}

sopWordsII = \lyricmode {
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
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
  
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Staff = women <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "altosIII"  \lyricsto "tenors" \sopWordsIII
    
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Oh My Love"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  tagline = ""
}}


