\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"God Be Merciful to Me"}}
  composer = \markup\oldStyleNum"J. P. Holbrook (1821–1889)"
  poet = \markup\oldStyleNum"Psalm 51"
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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key des \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 f8. f16 |
  f4. des8 ges8. f16 |
  f4( des) \bar""
  \times2/3{des8[ ees] f} |
  ges4. bes8 aes8. ges16 |
  f2 \bar""
  des8. des16 |
  ees4~ \times2/3{ees8[ d] ees} \times2/3{ees[ d] ees} |
  f4( des?) \bar""
  des8. des16 |
  ees4~ \times2/3{ees8[ d] ees} \times2/3{ees[ ges] f} |
  des?2 \bar""

  aes'8. aes16 |
  f'4. ees8 \times2/3{ees[ des] bes} |
  aes4( f) \bar""
  des'8. bes16 |
  aes4. f8 ees8. f16 |
  ges2\fermata \bar""
  f8. aes16 |
  f'4. ees8 \times2/3{ees[ des] bes} |
  aes4( f) \bar""
  des'8. bes16 |
  aes4. des,8 f8. ees16 |
  des2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  God, be mer -- ci -- ful to me; __
  on your grace I rest my plea. 
  My trans -- gres -- sions I __ con -- fess; __
  grief and guilt __ my soul op -- press. 
  Wash me, make me pure with -- in; __
  cleanse, O cleanse me from my sin.
  Wash me, make me pure with -- in; __
  cleanse, O cleanse me from my sin. 
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I have sinned a -- gainst your grace 
  and pro -- voked you to your face. 
  I con -- fess __ your judg -- ment just; __
  speech -- less, I __ your mer -- cy trust. 
  Let my con -- trite heart re -- joice __
  and in glad -- ness hear your voice. 
  Let my con -- trite heart re -- joice __
  and in glad -- ness hear your voice. 
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Gra -- cious God, my heart re -- new, 
  make my spir -- it right and true. 
  Do not cast __ me from your sight __
  nor re -- move __ your Spir -- it’s light. 
  Your sal -- va -- tion’s joy re -- store,
  make me stead -- fast ev -- er -- more. 
  Your sal -- va -- tion’s joy re -- store,
  make me stead -- fast ev -- er -- more. 
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Con -- trite spir -- it, plead -- ing cries, 
  you, O God, will not de -- spise. 
  Sin -- ful ways __ I will re -- prove, __
  and my tongue __ shall sing your love. 
  Let my right -- eous sac -- ri -- fice __
  then de -- light your ho -- ly eyes. 
  Let my right -- eous sac -- ri -- fice __
  then de -- light your ho -- ly eyes. 
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4 f8. f16 |
  f4. des8 ges8. f16 |
  f4( des) \bar""
  \times2/3{des8[ ees] f} |
  ges4. bes8 aes8. ges16 |
  f2 \bar""
  des8. des16 |
  ees4~ \times2/3{ees8[ d] ees} \times2/3{ees[ d] ees} |
  f4( des) \bar""
  des8. des16 |
  ees4~ \times2/3{ees8[ d] ees} \times2/3{ees[ ges] f} |
  des2 \bar""

  f8. f16 |
  aes4. ges8 \times2/3{ges4 ges8} |
  f4( des)

  f8. ges16 |
  f4. des8 c8. des16 |
  ees2

  des8. f16 |
  aes4. ges8 \times2/3{ges4 ges8} 
  f4( des)

  f8. ges16 |
  f4. des8 des8. c16 |
  des2 \bar"|."
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
  aes8. aes16 |
  aes4. f8 bes8. aes16 |
  aes4( f)

  \times2/3{f8[ ges] aes} |
  bes4. des8 c8. bes16 |
  aes2

  f8. f16 |
  ges4~ \times2/3{ges8[ f] ges} \times2/3{ges[ f] ges} |
  aes4( f)

  f8. f16 |
  ges4~ \times2/3{ges8[ f] ges} \times2/3{ges[ bes] aes} |
  f2

  aes8. aes16 |
  des4. c8 \times2/3{bes4 des8} |
  des4( aes)

  aes8. bes16 |
  des4. aes8 aes8. des16 |
  c2

  aes8. aes16 |
  des4. c8 \times2/3{bes4 des8} |
  des4( aes)

  aes8. bes16 |
  des4. f,8 aes8. ges16 |
  f2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {  
  aes8. aes16 |
  aes4. f8 bes8. aes16 |
  aes4( f)

  \times2/3{f8[ ges] aes} |
  bes4. des8 c8. bes16 |
  aes2

  f8. f16 |
  ges4~ \times2/3{ges8[ f] ges} \times2/3{ges[ f] ges} |
  aes4( f)

  f8. f16 |
  ges4~ \times2/3{ges8[ f] ges} \times2/3{ges[ bes] aes} |
  f2

  des8. des16 |
  des4. des8 \times2/3{ges4 ges,8} |
  des'2

  des8. des16 |
  des4. des8 aes8. aes16 |
  aes2\fermata

  des8. des16 |
  des4. des8 \times2/3{ges4 ges,8} |
  des'2

  des8. des16 |
  des4. des8 aes8. aes16 |
  des2 \bar"|."
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
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}
